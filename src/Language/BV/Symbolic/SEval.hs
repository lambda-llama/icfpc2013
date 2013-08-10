{-# LANGUAGE RecordWildCards #-}

module Language.BV.Symbolic.SEval where

import Data.List

import Language.BV.Symbolic.Types
import Language.BV.Types

seval :: BVExpr -> [(BVId, Sword)] -> Sword
seval e env = case e of
    Zero -> zero
    One  -> one
    Id x -> undefined
    If0  e0 e1 e2 ->
        let v0 = seval e0 env
            v1 = seval e1 env
            v2 = seval e2 env
        in case (isZero v0, isNotZero v0) of
            (True, _) -> v1
            (_, True) -> v2
            _ -> merge v1 v2
           
    Fold (BVFold { bvfLambda = (larg0, larg1, le0), .. }) ->
        bot
    Op1 op1 e0    ->
        bot
    Op2 op2 e0 e1 ->
        bot

zero :: Sword
zero = [Szero | _ <- [1..64]]

one :: Sword
one = Sone:(tail zero)

bot:: Sword
bot = [Bot | _ <- [1..64]]

isZero :: Sword -> Bool
isZero = (==zero)

isNotZero :: Sword -> Bool
isNotZero = any (==Sone)

merge :: Sword -> Sword -> Sword
merge = \_ _ -> bot



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
