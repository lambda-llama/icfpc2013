{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Eval where

import Data.Bits ((.&.), (.|.), complement, shift, xor, shiftR)
import Data.Word (Word64)

import qualified Data.Vector.Unboxed as VU

import Language.BV.Types


evalProgram :: BVProgram -> Word64 -> Word64
evalProgram (BVProgram (x, e)) v = evalExpr [(x, v)] e

evalExpr :: [(BVId, Word64)] -> BVExpr -> Word64
evalExpr env0 expr = go env0 expr where
  go _env Zero   = 0
  go _env One    = 1
  go !env (Id x) = case lookup x env of
      Nothing -> error $ x : " is not defined!"
      Just v  -> v
  go !env (If0 e0 e1 e2) =
      let !v0 = go env e0
          v1  = go env e1
          v2  = go env e2
      in if v0 == 0 then v1 else v2
  go !env (Fold (BVFold { bvfLambda = (larg0, larg1, le), .. })) =
      if isClosed le
      then go env le
      else
          let !i      = go env bvfInit
              !arg    = go env bvfArg
              !bytes  = VU.fromList
                        [(shiftR arg offset) .&. 0xff | offset <- [0,8..56]]
          in VU.foldl' (\b a -> evalExpr ((larg0, a):(larg1, b):env) le) i bytes
  go !env (Op1 op1 e0) =
      let !v0 = go env e0 in
      case op1 of
          Not   -> complement v0
          Shl1  -> shift v0 1
          Shr1  -> shift v0 (-1)
          Shr4  -> shift v0 (-4)
          Shr16 -> shift v0 (-16)
  go !env (Op2 op2 e0 e1) =
      let !v0 = go env e0
          !v1 = go env e1
      in case op2 of
          And  -> v0 .&. v1
          Or   -> v0 .|. v1
          Xor  -> v0 `xor` v1
          Plus -> v0 + v1
{-# INLINE evalExpr #-}
