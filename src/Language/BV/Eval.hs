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
evalExpr !env e = go e where
  go Zero   = 0
  go One    = 1
  go (Id x) = case lookup x env of
      Nothing -> error $ x : " is not defined!"
      Just v  -> v
  go (If0 e0 e1 e2) =
      let !v0 = go e0
          v1  = go e1
          v2  = go e2
      in if v0 == 0 then v1 else v2
  go (Fold (BVFold { bvfLambda = (larg0, larg1, le), .. })) =
      if isClosed le
      then go le
      else
          let !i      = go bvfInit
              !arg    = go bvfArg
              !bytes  = VU.fromList
                        [(shiftR arg offset) .&. 0xff | offset <- [0,8..56]]
          in VU.foldl' (\b a -> evalExpr ((larg0, a):(larg1, b):env) le) i bytes
  go (Op1 op1 e0) =
      let !v0 = go e0 in
      case op1 of
          Not   -> complement v0
          Shl1  -> shift v0 1
          Shr1  -> shift v0 (-1)
          Shr4  -> shift v0 (-4)
          Shr16 -> shift v0 (-16)
  go (Op2 op2 e0 e1) =
      let !v0 = go e0
          !v1 = go e1
      in case op2 of
          And  -> v0 .&. v1
          Or   -> v0 .|. v1
          Xor  -> v0 `xor` v1
          Plus -> v0 + v1
