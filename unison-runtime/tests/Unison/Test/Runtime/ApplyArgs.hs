{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Tests for 'nestEvalArgs', the re-entrant entry point that applies a
-- Unison function to arguments supplied from Haskell (as used by C
-- callbacks). We compile a small Unison lambda, recover its runtime 'Val',
-- apply it to argument values, and check the result — including an
-- order-sensitive builtin so a reversed-argument bug would be caught.
module Unison.Test.Runtime.ApplyArgs (test) where

import Control.Concurrent.STM (readTVarIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import EasyTest
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF (Cacheability (..), Code (..), lamLift, superNormalize)
import Unison.Runtime.MCode (CombIx (..))
import Unison.Runtime.Machine
  ( CCache (..),
    baseCCache,
    cacheAdd,
    mCombVal,
    nestEvalArgs,
    rCombSection,
    refNumTm,
  )
import Unison.Runtime.Pattern (builtinDataSpec, splitPatterns)
import Unison.Runtime.Stack (Val, pattern IntVal, pattern NatVal)
import Unison.Term (unannotate)
import Unison.Test.Common (tm)

mainRef :: Reference
mainRef = Builtin "main"

-- Compile a closed Unison term to its runtime function 'Val'.
compileVal :: String -> IO (CCache (), Val)
compileVal s = do
  cc <- baseCCache False
  let (ll, _, _, ctx, _) =
        lamLift mempty
          . splitPatterns builtinDataSpec
          . unannotate
          $ tm s
      env = (mainRef, superNormalize ll) : (fmap superNormalize <$> ctx)
      uncacheable sg = CodeRep sg Uncacheable
  _ <- cacheAdd ((fmap . fmap) uncacheable env) cc
  w <- refNumTm cc mainRef
  cmbs <- readTVarIO (combs cc)
  let cix = CIx mainRef w 0
  pure (cc, mCombVal cix (rCombSection cmbs cix))

-- Apply the compiled function to the given argument values and read back
-- the resulting value.
applyN :: String -> [Val] -> IO Val
applyN s args = do
  (cc, f) <- compileVal s
  out <- newIORef Nothing
  nestEvalArgs cc Nothing args (writeIORef out . Just) f
  readIORef out >>= \case
    Just v -> pure v
    Nothing -> error "nestEvalArgs did not produce a result"

expectNat :: Word64 -> Val -> Test ()
expectNat expected = \case
  NatVal n -> expectEqual expected n
  v -> crash $ "expected a Nat, got: " ++ show v

expectInt :: Int -> Val -> Test ()
expectInt expected = \case
  IntVal n -> expectEqual expected n
  v -> crash $ "expected an Int, got: " ++ show v

test :: Test ()
test =
  scope "apply-args" . tests $
    [ scope "unary" do
        r <- io $ applyN "x -> ##Nat.+ x 1" [NatVal 41]
        expectNat 42 r,
      -- order-sensitive: ##Nat.sub 10 3 = +7, while reversed it would be -7
      -- (##Nat.sub : Nat -> Nat -> Int)
      scope "binary order" do
        r <- io $ applyN "x y -> ##Nat.sub x y" [NatVal 10, NatVal 3]
        expectInt 7 r,
      scope "ternary" do
        r <- io $ applyN "x y z -> ##Nat.sub (##Nat.sub x y) z" [NatVal 100, NatVal 30, NatVal 8]
        expectInt 62 r
    ]
