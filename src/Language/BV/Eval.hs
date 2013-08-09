module Language.BV.Eval where

import Data.Int (Int64)

import Language.BV.Types


evalBv :: BVProgram -> Int64 -> Int64
evalBv (BVProgram (x, e)) v = evalBvExpr e [(x, v)]

evalBvExpr :: BVExpr -> [(BVId, Int64)] -> Int64
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

evalOp1 = undefined

evalOp2 = undefined
