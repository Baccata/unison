{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests for Unison -> C callbacks: compile a Unison function,
-- turn it into a C function pointer with 'mkCallback', then invoke that
-- pointer through libffi (as a C caller would) and check the result. This
-- exercises the whole trampoline — argument decoding, re-entering the
-- evaluator, and result encoding — across integer, signed-result, and
-- floating-point (with F32 truncation) signatures.
module Unison.Test.Runtime.Foreign.Callback (test) where

import Data.Int (Int64)
import Data.Word (Word64)
import EasyTest
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable (Storable, peek)
import Unison.Runtime.Foreign.Callback (freeCallback, mkCallback)
import Unison.Runtime.Foreign.Dynamic (CDynFunc, FFSpec (..), FFType (..), callForeign)
import Unison.Runtime.Machine (nestEvalArgs)
import Unison.Test.Runtime.ApplyArgs (compileVal)

-- Build a callback from a Unison function with the given C signature.
callbackFor :: FFSpec -> String -> IO CDynFunc
callbackFor spec src = do
  (cc, fval) <- compileVal src
  let run args write = nestEvalArgs cc Nothing args write fval
  mkCallback spec run

-- Invoke a prepared callback with two same-typed scalar arguments, reading the
-- result as type @r@. Arguments and result occupy single 8-byte slots.
call2 :: (Storable a, Storable r) => CDynFunc -> a -> a -> IO r
call2 cb a b =
  with a \pa -> with b \pb ->
    withArray [castPtr pa, castPtr pb] \argv ->
      allocaBytes 8 \ret -> do
        callForeign cb argv ret
        peek (castPtr ret)

call1 :: (Storable a, Storable r) => CDynFunc -> a -> IO r
call1 cb a =
  with a \pa ->
    withArray [castPtr pa] \argv ->
      allocaBytes 8 \ret -> do
        callForeign cb argv ret
        peek (castPtr ret)

test :: Test ()
test =
  scope "ffi.callback" . tests $
    [ scope "unary Nat callback" do
        r <- io do
          cb <- callbackFor (FFSpec [U64] U64 Nothing) "x -> ##Nat.increment x"
          r <- call1 cb (41 :: Word64) :: IO Word64
          freeCallback cb
          pure r
        expectEqual 42 r,
      -- order-sensitive, signed result: ##Nat.sub 10 3 = +7 (reversed: -7)
      scope "binary Nat->Int callback" do
        r <- io do
          cb <- callbackFor (FFSpec [U64, U64] I64 Nothing) "x y -> ##Nat.sub x y"
          r <- call2 cb (10 :: Word64) (3 :: Word64) :: IO Int64
          freeCallback cb
          pure r
        expectEqual 7 r,
      -- exercises F32 decode/encode (including the float<->double conversion)
      scope "binary Float32 callback" do
        r <- io do
          cb <- callbackFor (FFSpec [F32, F32] F32 Nothing) "x y -> ##Float.+ x y"
          r <- call2 cb (1.5 :: Float) (2.25 :: Float) :: IO Float
          freeCallback cb
          pure r
        expectEqual 3.75 r,
      scope "freeCallback is idempotent" do
        ok <- io do
          cb <- callbackFor (FFSpec [U64] U64 Nothing) "x -> ##Nat.increment x"
          freeCallback cb
          freeCallback cb
          pure True
        expect ok
    ]
