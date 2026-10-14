{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}

module Unison.Test.Runtime.Foreign.Dynamic (test) where

import Control.Exception (try)
import Control.Monad (replicateM_, void)
import EasyTest
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import System.Mem (performGC)
import Unison.Runtime.Foreign.Dynamic

-- | Whether an unpromoted /fixed/ prefix is accepted. Not checked on macOS:
-- Apple's system libffi applies the "variadic arguments must be promoted" check
-- to every argument past the first, ignoring @nfixedargs@, and so rejects a
-- legal unpromoted fixed prefix with @FFI_BAD_ARGTYPE@. It does this even when
-- @nfixedargs@ equals the total argument count and nothing is variadic at all.
-- Upstream libffi accepts these specs; @\/usr\/lib\/libffi.dylib@ is what GHC
-- links against here.
promotionTests :: [Test ()]
#if defined(darwin_HOST_OS)
promotionTests = []
#else
promotionTests =
  [ scope "fixed prefix is not promoted" do
      actual <- io $ try $ void $ prepareSpec $ FFSpec [F32, I8, U16, D64, I32] Void (Just 3)
      expectEqual (Right () :: Either PrepException ()) actual
  ]
#endif

foreign import ccall unsafe "&snprintf" snprintfPtr :: FunPtr ()

foreign import ccall unsafe "&strlen" strlenPtr :: FunPtr ()

foreign import ccall unsafe "&qsort" qsortPtr :: FunPtr ()

foreign import ccall "wrapper"
  mkIntCompare ::
    (Ptr CInt -> Ptr CInt -> IO CInt) ->
    IO (FunPtr (Ptr CInt -> Ptr CInt -> IO CInt))

-- Ascending integer comparator for qsort, returning -1/0/1.
intCompare :: Ptr CInt -> Ptr CInt -> IO CInt
intCompare a b = do
  x <- peek a
  y <- peek b
  pure . fromIntegral $ fromEnum (compare x y) - 1

data Argument = forall a. (Storable a) => Argument a

withArguments :: [Argument] -> (Ptr (Ptr ()) -> IO a) -> IO a
withArguments args action = go args []
  where
    go [] ptrs = withArray (reverse ptrs) action
    go (Argument x : xs) ptrs = with x \p -> go xs (castPtr p : ptrs)

sizeType :: FFType
sizeType = if sizeOf (undefined :: CSize) == 8 then U64 else U32

-- Reuse a prepared interface after other preparations and GC. In particular,
-- the argument-type array must not be a temporary alloca in prepareSpec.
formatWith :: [FFType] -> [Argument] -> String -> IO (String, CInt)
formatWith types values format = do
  spec <- prepareSpec $ FFSpec ([MBArr, sizeType, Ptr] ++ types) I32 (Just 3)
  let fun = CDynFunc "snprintf" spec snprintfPtr (pure ())
  replicateM_ 100 $ void $ prepareSpec $ FFSpec [D64, U64, I32, Ptr] D64 Nothing
  performGC
  allocaBytes 256 \buffer ->
    withCString format \fmt ->
      withArguments ([Argument buffer, Argument (256 :: CSize), Argument fmt] ++ values) \args ->
        allocaBytes 8 \result -> do
          replicateM_ 20 $ callForeign fun args result
          (,) <$> peekCString buffer <*> peek (castPtr result)

test :: Test ()
test =
  scope "ffi.dynamic" $
    tests $
      [ scope "fixed calls" do
          actual <- io do
            spec <- prepareSpec $ FFSpec [Ptr] sizeType Nothing
            performGC
            withCString "fixed FFI" \str ->
              withArguments [Argument str] \args ->
                allocaBytes 8 \result -> do
                  callForeign (CDynFunc "strlen" spec strlenPtr (pure ())) args result
                  peek (castPtr result) :: IO CSize
          expectEqual 9 actual,
        scope "function identity includes the call signature" do
          actual <- io do
            let make name spec = CDynFunc name <$> prepareSpec spec <*> pure snprintfPtr <*> pure (pure ())
                args = [MBArr, sizeType, Ptr]
                spec = FFSpec args I32 (Just 3)
            original <- make "snprintf" spec
            same <- make "another name for snprintf" spec
            otherArity <- make "snprintf" (FFSpec (args ++ [I32]) I32 (Just 3))
            otherType <- make "snprintf" (FFSpec (args ++ [I64]) I32 (Just 3))
            fixed <- make "snprintf" (FFSpec args I32 Nothing)
            pure
              ( original == same && compare original same == EQ,
                all
                  (\(l, r) -> l /= r && compare l r /= EQ)
                  [(original, otherArity), (otherArity, otherType), (original, fixed)]
              )
          expectEqual (True, True) actual,
        scope "no optional arguments" do
          actual <- io $ formatWith [] [] "no extras"
          expectEqual ("no extras", 9) actual,
        scope "mixed variadic arguments and register overflow" do
          actual <- io $ withCString "hello" \str ->
            formatWith
              [I32, Ptr, D64, U32, I64, U64, D64, I32, D64, I32, D64, I32, D64, I32, D64]
              [ Argument (-7 :: CInt),
                Argument str,
                Argument (2.5 :: CDouble),
                Argument (99 :: CUInt),
                Argument (-1234567890123 :: CLLong),
                Argument (12345678901234 :: CULLong),
                Argument (3.5 :: CDouble),
                Argument (4 :: CInt),
                Argument (4.5 :: CDouble),
                Argument (5 :: CInt),
                Argument (5.5 :: CDouble),
                Argument (6 :: CInt),
                Argument (6.5 :: CDouble),
                Argument (7 :: CInt),
                Argument (7.5 :: CDouble)
              ]
              "%d %s %.1f %u %lld %llu %.1f %d %.1f %d %.1f %d %.1f %d %.1f"
          let expected = "-7 hello 2.5 99 -1234567890123 12345678901234 3.5 4 4.5 5 5.5 6 6.5 7 7.5"
          expectEqual (expected, fromIntegral $ length expected) actual,
        scope "void placeholder for fixed no-argument function" do
          actual <- io $ ffArgs . ffSpec <$> prepareSpec (FFSpec [Void] I32 Nothing)
          expectEqual [] actual,
        scope "reject invalid fixed counts" $
          tests
            [ rejects (BadFixedArgs n 2) (FFSpec [I32, D64] I32 (Just n))
            | n <- [0, 3, 4294967296, maxBound]
            ],
        scope "reject unpromoted optional arguments" $
          tests
            [ rejects (BadVariadicArg 2 t) (FFSpec [I32, t] I32 (Just 1))
            | t <- [F32, I8, U8, I16, U16]
            ],
        scope "reject void arguments" $
          tests
            [ rejects BadVoid (FFSpec [I32, Void] I32 (Just 1)),
              rejects BadVoid (FFSpec [Void] I32 (Just 1)),
              rejects BadVoid (FFSpec [I32, Void] I32 Nothing)
            ],
        scope "reject array results" $
          rejects BadResult (FFSpec [I32] MBArr (Just 1)),
        scope "accept function-pointer argument" do
          actual <- io $ try $ void $ prepareSpec $ FFSpec [Fun, I32] I32 Nothing
          expectEqual (Right () :: Either PrepException ()) actual,
        scope "reject function-pointer result" $
          rejects BadResult (FFSpec [I32] Fun Nothing),
        scope "function-pointer argument is passed and invoked (qsort)" do
          actual <- io do
            cmp <- mkIntCompare intCompare
            spec <- prepareSpec $ FFSpec [Ptr, sizeType, sizeType, Fun] Void Nothing
            let fun = CDynFunc "qsort" spec qsortPtr (pure ())
                input = [5, 3, 8, 1, 9, 2] :: [CInt]
                n = length input
                elemSize = sizeOf (undefined :: CInt)
            withArray input \arr ->
              withArguments
                [ Argument arr,
                  Argument (fromIntegral n :: CSize),
                  Argument (fromIntegral elemSize :: CSize),
                  Argument cmp
                ]
                \args ->
                  allocaBytes 8 \result -> do
                    callForeign fun args result
                    sorted <- peekArray n arr
                    freeHaskellFunPtr cmp
                    pure sorted
          expectEqual [1, 2, 3, 5, 8, 9] actual
      ]
        <> promotionTests
  where
    rejects expected spec = do
      actual <- io $ try $ void $ prepareSpec spec
      expectEqual (Left expected) actual
