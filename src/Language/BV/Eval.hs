module Language.BV.Eval where

import Data.Word (Word64)
import Data.Bits ((.&.), (.|.), complement, shift, xor)

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
    Op1 op1 e0    ->
        let v0 = evalBvExpr e0 env in
        case op1 of
            Not   -> complement v0
            Shl1  -> shift v0 1
            Shr1  -> shift v0 (-1)
            Shr4  -> shift v0 (-4)
            Shr16 -> shift v0 (-16)
    Op2 op2 e0 e1 ->
        let v0 = evalBvExpr e0 env
            v1 = evalBvExpr e1 env
        in case op2 of
            And  -> v0 .&. v1
            Or   -> v0 .|. v1
            Xor  -> v0 `xor` v1
            Plus -> v0 + v1
