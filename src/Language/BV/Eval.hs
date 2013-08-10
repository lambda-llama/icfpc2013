{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Eval where

import Data.Bits ((.&.), (.|.), complement, shift, xor, shiftR)
import Data.List (foldl')
import Data.Word (Word64)

import Language.BV.Types


evalProgram :: BVProgram -> Word64 -> Word64
evalProgram (BVProgram (x, e)) v = evalExpr e [(x, v)]

evalExpr :: BVExpr -> [(BVId, Word64)] -> Word64
evalExpr e !env = case e of
    Zero -> 0
    One  -> 1
    Id x -> case lookup x env of
        Nothing -> error (x : " is not defined in " ++ show e ++
                          ", env: " ++ show env)
        Just v  -> v
    If0 e0 e1 e2 ->
        let !v0 = evalExpr e0 env
            !v1 = evalExpr e1 env
            !v2 = evalExpr e2 env
        in if v0 == 0 then v1 else v2
    Fold (BVFold { bvfLambda = (larg0, larg1, le), .. }) ->
        if isClosed le
        then evalExpr le env
        else
            let !i      = evalExpr bvfInit env
                !arg    = evalExpr bvfArg env
                !bytes  = [(shiftR arg offset) .&. 0xff | offset <- [0,8..56]]
            in foldl' (\b a -> evalExpr le ((larg0, a):(larg1, b):env)) i bytes
    Op1 op1 e0    ->
        let !v0 = evalExpr e0 env in
        case op1 of
            Not   -> complement v0
            Shl1  -> shift v0 1
            Shr1  -> shift v0 (-1)
            Shr4  -> shift v0 (-4)
            Shr16 -> shift v0 (-16)
    Op2 op2 e0 e1 ->
        let !v0 = evalExpr e0 env
            !v1 = evalExpr e1 env
        in case op2 of
            And  -> v0 .&. v1
            Or   -> v0 .|. v1
            Xor  -> v0 `xor` v1
            Plus -> v0 + v1
