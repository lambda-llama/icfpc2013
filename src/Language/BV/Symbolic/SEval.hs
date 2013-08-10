{-# LANGUAGE RecordWildCards #-}

module Language.BV.Symbolic.SEval where

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



liftop1 :: (a -> a) -> (Sbit -> Sbit)
liftop1 = undefined

liftop2 :: (a -> a -> a) -> (Sbit -> Sbit-> Sbit)
liftop2 = undefined


not   = undefined
shl1  = undefined
shr1  = undefined
shr4  = undefined
shr16 = undefined
    
and  = undefined
or   = undefined
xor  = undefined
plus = undefined
