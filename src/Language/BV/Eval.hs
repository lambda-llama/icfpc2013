{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Eval where

import Data.Bits ((.&.), (.|.), complement, shift, xor, shiftR, shiftL)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)

import qualified Data.Vector.Unboxed as VU

import Language.BV.Types

evalProgram :: BVProgram -> Word64 -> Word64
evalProgram (BVProgram (x, e)) v = evalExpr [(x, v)] e
{-# INLINE evalProgram #-}

evalExpr :: [(BVId, Word64)] -> BVExpr -> Word64
evalExpr env0 expr = go env0 expr where
  go _env Zero  = 0
  go _env One   = 1
  go env (Id x) = fromMaybe (error $ x : " is not defined!") $ lookup x env
  go env (If0 e0 e1 e2) =
      let !v0 = go env e0
          v1  = go env e1
          v2  = go env e2
      in if v0 == 0 then v1 else v2
  go env (Fold (BVFold { bvfLambda = (larg0, larg1, le), .. })) =
      if isClosed le
      then go env le
      else
          let !i      = go env bvfInit
              !arg    = go env bvfArg
              !bytes  = VU.fromList
                        [shiftR arg offset .&. 0xff | offset <- [0,8..56]]
          in VU.foldl' (\b a -> go ((larg0, a):(larg1, b):env) le) i bytes
  go env e@(Op1 op1 e0) =
      case op1 of
          Not   -> complement $! go env e
          Shl1  -> fastShiftLeft1 env e
          Shr1  -> fastShiftRight env e
          Shr4  -> fastShiftRight env e
          Shr16 -> fastShiftRight env e
  go env (Op2 op2 e0 e1) =
      let !v0 = go env e0
          !v1 = go env e1
      in case op2 of
          And  -> v0 .&. v1
          Or   -> v0 .|. v1
          Xor  -> v0 `xor` v1
          Plus -> v0 + v1

  fastShiftLeft1 env (Op1 Shl1 (Op1 Shl1 (Op1 Shl1 (Op1 Shl1 e)))) = go env e `shiftL` 4
  fastShiftLeft1 env (Op1 Shl1 (Op1 Shl1 (Op1 Shl1 e))) = go env e `shiftL` 2
  fastShiftLeft1 env (Op1 Shl1 (Op1 Shl1 e)) = go env e `shiftL` 2
  fastShiftLeft1 env (Op1 Shl1 e) = go env e `shiftL` 1
  fastShiftLeft1 env e = go env e

  fastShiftRight env (Op1 Shr16 (Op1 Shr16 (Op1 Shr16 e))) = go env e `shiftR` 48
  fastShiftRight env (Op1 Shr16 (Op1 Shr16 e)) = go env e `shiftR` 32
  fastShiftRight env (Op1 Shr16 e) = go env e `shiftR` 16
  fastShiftRight env (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 (Op1 Shr16 e)))) = go env e `shiftR` 28
  fastShiftRight env (Op1 Shr4 (Op1 Shr4 (Op1 Shr16 e))) = go env e `shiftR` 24
  fastShiftRight env (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 e))) = go env e `shiftR` 12
  fastShiftRight env (Op1 Shr4 (Op1 Shr16 e)) = go env e `shiftR` 20
  fastShiftRight env (Op1 Shr4 (Op1 Shr4 e)) = go env e `shiftR` 8
  fastShiftRight env (Op1 Shr4 e) = go env e `shiftR` 4
  fastShiftRight env (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 (Op1 Shr16 e)))) = go env e `shiftR` 19
  fastShiftRight env (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 (Op1 Shr4 e)))) = go env e `shiftR` 7
  fastShiftRight env (Op1 Shr1 (Op1 Shr1 (Op1 Shr16 e))) = go env e `shiftR` 18
  fastShiftRight env (Op1 Shr1 (Op1 Shr1 (Op1 Shr4 e))) = go env e `shiftR` 6
  fastShiftRight env (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 e))) = go env e `shiftR` 3
  fastShiftRight env (Op1 Shr1 (Op1 Shr16 e)) = go env e `shiftR` 17
  fastShiftRight env (Op1 Shr1 (Op1 Shr4 e)) = go env e `shiftR` 5
  fastShiftRight env (Op1 Shr1 (Op1 Shr1 e)) = go env e `shiftR` 2
  fastShiftRight env (Op1 Shr1 e) = go env e `shiftR` 1
  fastShiftRight env e = go env e
{-#NLINE evalExpr #-}
