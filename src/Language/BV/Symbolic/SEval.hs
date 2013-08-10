{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Symbolic.SEval where

import Data.Function (on)

import Language.BV.Symbolic.Types
import Language.BV.Symbolic.Operations
import Language.BV.Types

sevalExpr :: [(BVId, Sword)] -> BVExpr -> Sword
sevalExpr !env = go where
  go Zero = zero
  go One  = one
  go (Id x) = case lookup x env of
      Nothing -> error (x : " is not defined!")
      Just v  -> v
  go (If0 e0 e1 e2) =
      let !v0 = go e0
          v1  = go e1
          v2  = go e2
      in case (isZero v0, isNotZero v0) of
          (True, _) -> v1
          (_, True) -> v2
          _ -> v1 `merge` v2
  go (Fold (BVFold { bvfLambda = (larg0, larg1, le0), .. })) =
      let init_ = go bvfInit
          arg = go bvfArg
          bytes = [(drop 8 zero) ++ take 8 (drop (64 - 8 -shift) arg)| shift <- [0,8..56]]
          aux b a = sevalExpr ((larg0, a):(larg1, b):env) le0
      in foldl aux init_ bytes
  go (Op1 op1 e0) =
      let !v0 = go e0 in
      case op1 of
          Not   -> snot v0
          Shl1  -> sshl1 v0
          Shr1  -> sshr1 v0
          Shr4  -> sshr4 v0
          Shr16 -> sshr16 v0
  go (Op2 op2 e0 e1) =
      let !v0 = go e0
          !v1 = go e1
      in case op2 of
          And  -> v0 `sand` v1
          Or   -> v0 `sor` v1
          Xor  -> v0 `sxor` v1
          Plus -> v0 `splus` v1

stdContext :: [(BVId, Sword)]
stdContext = [('x', inputx), ('y', inputy), ('z', inputz)] where
  inputx = [B i | i <- [1..64]]
  inputy = [B (65 + i) | i <- [0..63]]
  inputz = [B (130 + i) | i <- [0..63]]

like :: BVExpr -> BVExpr -> Bool
like e0 e1 = e0 == e1 || (slike `on` sevalExpr stdContext) e0 e1
