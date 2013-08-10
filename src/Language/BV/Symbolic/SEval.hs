{-# LANGUAGE RecordWildCards #-}

module Language.BV.Symbolic.SEval where

import Data.List

import Language.BV.Symbolic.Types
import Language.BV.Types

seval :: BVExpr -> [(BVId, Sword)] -> Sword
seval e env = case e of
    Zero -> zero
    One  -> one
    Id x -> case lookup x env of
        Nothing -> error (x : " is not defined(Simbolic)!")
        Just v  -> v
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
        let v0 = seval e0 env in
        case op1 of
            Not   -> snot v0
            Shl1  -> sshl1 v0
            Shr1  -> sshr1 v0
            Shr4  -> sshr4 v0
            Shr16 -> sshr16 v0

    Op2 op2 e0 e1 ->
        let v0 = seval e0 env
            v1 = seval e1 env
        in case op2 of
            And  -> v0 `sand` v1
            Or   -> v0 `sor` v1
            Xor  -> v0 `sxor` v1
            Plus -> v0 `splus` v1


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
merge a b = map (uncurry lb) (zip a b)


snot = map comp
sshl1 (x:sw) = sw ++ [Szero]
sshr1 sw = Szero : (init sw)
sshr4 = sshr1 . sshr1 . sshr1 . sshr1
sshr16 = sshr4 . sshr4 . sshr4 . sshr4


and_bit (a, Szero)      = Szero
and_bit (Szero, a)      = Szero
and_bit (a, Sone)       = a
and_bit (Sone, a)       = a
and_bit ((B i), (B j)) | i == j    = (B i)
                       | i == -j   = Szero
                       | otherwise = Bot
and_bit (Bot, Bot)     = Bot

or_bit (a, Szero)       = a
or_bit (Szero, a)       = a
or_bit (a, Sone)        = Sone
or_bit (Sone, a)        = Sone
or_bit ((B i), (B j))  | i == j    = (B i)
                       | i == -j   = Sone
                       | otherwise = Bot
or_bit (Bot, Bot)      = Bot

xor_bit (a, Szero)      = a
xor_bit (Szero, a)      = a
xor_bit (a, Sone)       = comp a
xor_bit (Sone, a)       = comp a
xor_bit ((B i), (B j)) | i == j    = Szero
                       | i == -j   = Sone
                       | otherwise = Bot
xor_bit (Bot, Bot)     = Bot

plus_bit :: (Sword, Sbit) -> (Sbit, Sbit) -> (Sword, Sbit)
plus_bit (acc, t) (a, b) = ((xor_bit (xab, t)) : acc, or_bit (oa, abt))
    where xab = xor_bit (a, b)
          aab = and_bit (a, b)
          aat = and_bit (a, t)
          abt = and_bit (b, t)
          oa  = or_bit (aab, aat)

sand :: Sword -> Sword -> Sword
sand a b= map and_bit $ zip a b

sor :: Sword -> Sword -> Sword
sor  a b= map or_bit $ zip a b

sxor :: Sword -> Sword -> Sword
sxor a b= map xor_bit $ zip a b

splus :: Sword -> Sword -> Sword
splus a b = fst . foldl' plus_bit ([], Szero)$ zip a b
