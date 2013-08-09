module Language.BV.Eval where

import Data.Word (Word64)
import Data.Bits
import Language.BV.Types


evalBv :: BVProgram -> Word64 -> Word64
evalBv (BVProgram (x, e)) v = evalBvExpr e [(x, v)]

evalBvExpr :: BVExpr -> [(BVId, Word64)] -> Word64
evalBvExpr e env = case e of
    Zero -> 0
    One  -> 1
    Id x -> case lookup x env of
        Nothing -> error (x ++ " is not defined!")
        Just v  -> v
    If0  e0 e1 e2 ->
        let v0 = evalBvExpr e0 env
            v1 = evalBvExpr e1 env
            v2 = evalBvExpr e2 env
        in if v0 == 0 then v1 else v2
    Fold f        -> undefined
    Op1 op1 e1    -> evalOp1 op1 e1 env
    Op2 op2 e1 e2 -> evalOp2 op2 e1 e2 env

evalOp1 :: BVOp1 -> BVExpr -> [(BVId, Word64)] -> Word64
evalOp1 Not e env = complement $ evalBvExpr e env
evalOp1 Shl1 e env = shift (evalBvExpr e env) 1
evalOp1 Shr1 e env = shift (evalBvExpr e env) (-1)
evalOp1 Shr4 e env = shift (evalBvExpr e env) (-4)
evalOp1 Shr16 e env = shift (evalBvExpr e env) (-16)

evalOp2 :: BVOp2 -> BVExpr -> BVExpr -> [(BVId, Word64)] -> Word64
evalOp2 And e1 e2 env = (evalBvExpr e1 env) .&. (evalBvExpr e2 env)
evalOp2 Or e1 e2 env = (evalBvExpr e1 env) .|. (evalBvExpr e2 env)
evalOp2 Xor e1 e2 env = xor (evalBvExpr e1 env) (evalBvExpr e2 env)
evalOp2 Plus e1 e2 env = (evalBvExpr e1 env) + (evalBvExpr e2 env)
