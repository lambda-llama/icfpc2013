module Language.BV.Symbolic.SEval where

import Language.BV.Symbolic.Types
import Language.BV.Types


-- evalExpr e env = case e of
--     Zero -> 0
--     One  -> 1
--     Id x -> case lookup x env of
--         Nothing -> error (x : " is not defined!")
--         Just v  -> v
--     If0  e0 e1 e2 ->
--         let v0 = evalExpr e0 env
--             v1 = evalExpr e1 env
--             v2 = evalExpr e2 env
--         in if v0 == 0 then v1 else v2
--     Fold (BVFold { bvfLambda = (larg0, larg1, le0), .. }) ->
--         let init_ = evalExpr bvfInit env
--             arg = evalExpr bvfArg env
--             byte = 0xff
--             bytes = [ (shiftR arg shift) .&. byte | shift <- [0,8..56]]
--             aux b a = evalExpr le0 ((larg0, a):(larg1, b):env)
--         in foldl aux init_ bytes
--     Op1 op1 e0    ->
--         let v0 = evalExpr e0 env in
--         case op1 of
--             Not   -> complement v0
--             Shl1  -> shift v0 1
--             Shr1  -> shift v0 (-1)
--             Shr4  -> shift v0 (-4)
--             Shr16 -> shift v0 (-16)
--     Op2 op2 e0 e1 ->
--         let v0 = evalExpr e0 env
--             v1 = evalExpr e1 env
--         in case op2 of
--             And  -> v0 .&. v1
--             Or   -> v0 .|. v1
--             Xor  -> v0 `xor` v1
--             Plus -> v0 + v1

liftop1 :: (a -> a) -> (Sbit -> Sbit)
liftop1 = undefined

liftop2 :: (a -> a -> a) -> (Sbit -> Sbit-> Sbit)
liftop2 = undefined

seval :: BVProgram -> Sword
seval = undefined


not   = undefined
shl1  = undefined
shr1  = undefined
shr4  = undefined
shr16 = undefined
    
and  = undefined
or   = undefined
xor  = undefined
plus = undefined
