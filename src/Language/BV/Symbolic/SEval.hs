module Language.BV.Symbolic.SEval where

import Data.List

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

liftop2 :: (a -> a -> a) -> (Sbit -> Sbit-> Sbit)
liftop2 = undefined

seval :: BVProgram -> Sword
seval = undefined

inv Zero  = One
inv One   = Zero
inv (B i) = B (-i)
inv Bot   = Bot

not sw = map inv sw
shl1 (x:sw) = sw ++ [Zero]
shr1 sw = Zero : (init sw)
shr4 sw = shr1 $ shr1 $ shr1 $ shr1 sw
shr16 sw = shr4 $ shr4 $ shr4 $ shr4 sw


and_bit (a, Zero)      = Zero
and_bit (Zero, a)      = Zero
and_bit (a, One)       = a
and_bit (One, a)       = a
and_bit ((B i), (B j)) | i == j    = (B i)
                       | i == -j   = Zero
                       | otherwise = Bot
and_bit (Bot, Bot)     = Bot

or_bit (a, Zero)       = a
or_bit (Zero, a)       = a
or_bit (a, One)        = One
or_bit (One, a)        = One
or_bit ((B i), (B j))  | i == j    = (B i)
                       | i == -j   = One
                       | otherwise = Bot
or_bit (Bot, Bot)      = Bot

xor_bit (a, Zero)      = a
xor_bit (Zero, a)      = a
xor_bit (a, One)       = inv a
xor_bit (One, a)       = inv a
xor_bit ((B i), (B j)) | i == j    = Zero
                       | i == -j   = One
                       | otherwise = Bot
xor_bit (Bot, Bot)     = Bot

plus_bit (a, b) (acc, t) = ((xor_bit (xab, t)) : acc, or_bit (oa, abt))
    where xab = xor_bit (a, b)
          aab = and (a, b)
          aat = and (a, t)
          abt = and (b, t)
          oa  = or (aab, aat)

and = map and_bit . zip
or  = map or_bit . zip
xor = map xor_bit . zip
plus = foldl` ([], Zero) plus_bit . zip