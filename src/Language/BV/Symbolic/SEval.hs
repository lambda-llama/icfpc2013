{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Symbolic.SEval where

import Data.Function (on)
import Data.List (foldl')

import qualified Data.Vector as V

import Language.BV.Symbolic.Types
import Language.BV.Symbolic.Operations
import Language.BV.Types

sevalExpr :: [(BVId, Sword)] -> BVExpr -> Sword
sevalExpr env0 expr = go env0 expr where
  go _env Zero   = zero
  go _env One    = one
  go !env (Id x) = case lookup x env of
      Nothing -> error (x : " is not defined!")
      Just v  -> v
  go !env (If0 e0 e1 e2) =
      let !v0 = go env e0
          v1  = go env e1
          v2  = go env e2
      in case (isZero v0, isNotZero v0) of
          (True, _) -> v1
          (_, True) -> v2
          _         -> v1 `merge` v2
  go !env (Fold (BVFold { bvfLambda = (larg0, larg1, le0), .. })) =
      let !i     = go env bvfInit
          !arg   = go env bvfArg
          !bytes = [ V.drop 8 zero V.++ V.take 8 (V.drop (64 - 8 - shift) arg)
                   | shift <- [0,8..56]
                   ]
      in foldl' (\b a -> sevalExpr ((larg0, a):(larg1, b):env) le0) i bytes
  go !env (Op1 op1 e0) =
      let !v0 = go env e0 in
      case op1 of
          Not   -> snot v0
          Shl1  -> sshl1 v0
          Shr1  -> sshr1 v0
          Shr4  -> sshr4 v0
          Shr16 -> sshr16 v0
  go !env (Op2 op2 e0 e1) =
      let !v0 = go env e0
          !v1 = go env e1
      in case op2 of
          And  -> v0 `sand` v1
          Or   -> v0 `sor` v1
          Xor  -> v0 `sxor` v1
          Plus -> v0 `splus` v1
{-# INLINEABLE sevalExpr #-}

stdContext :: [(BVId, Sword)]
stdContext = [('x', inputx), ('y', inputy), ('z', inputz)] where
  inputx = V.fromList [B i | i <- [1..64]]
  inputy = V.fromList [B (65 + i) | i <- [0..63]]
  inputz = V.fromList [B (130 + i) | i <- [0..63]]
{-# INLINE stdContext #-}

like :: BVExpr -> BVExpr -> Bool
like !e0 !e1 = e0 == e1 || (slike `on` sevalExpr stdContext) e0 e1
{-# INLINE like #-}
