module Language.BV.Symbolic.Operations where

import qualified Data.Vector.Unboxed as VU

import Language.BV.Symbolic.Types

zero :: Sword
zero = take 64 $ repeat Szero

one :: Sword
one = (tail zero) ++ [Sone]

bot:: Sword
bot = take 64 $ repeat Bot

isZero :: Sword -> Bool
isZero = (== zero)

isNotZero :: Sword -> Bool
isNotZero = any (== Sone)

merge :: Sword -> Sword -> Sword
merge a b = map (uncurry lb) (zip a b)

snot :: Sword -> Sword
snot = map complementSbit

sshl1 :: Sword -> Sword
sshl1 (_:sw) = sw ++ [Szero]
sshl1 _ = error "Empty Sword o_O!"

sshr1 :: Sword -> Sword
sshr1 sw = Szero : (init sw)

sshr4 :: Sword -> Sword
sshr4 = sshr1 . sshr1 . sshr1 . sshr1

sshr16 :: Sword -> Sword
sshr16 = sshr4 . sshr4 . sshr4 . sshr4

and_bit :: (Sbit, Sbit) -> Sbit
and_bit (_, Szero)      = Szero
and_bit (Szero, _)      = Szero
and_bit (a, Sone)       = a
and_bit (Sone, a)       = a
and_bit ((B i), (B j)) | i == j    = B i
                       | i == -j   = Szero
                       | otherwise = Bot
and_bit (_, Bot)     = Bot
and_bit (Bot, _)     = Bot

or_bit :: (Sbit, Sbit) -> Sbit
or_bit (a, Szero)       = a
or_bit (Szero, a)       = a
or_bit (_, Sone)        = Sone
or_bit (Sone, _)        = Sone
or_bit ((B i), (B j))  | i == j    = B i
                       | i == -j   = Sone
                       | otherwise = Bot
or_bit (_, Bot)      = Bot
or_bit (Bot, _)      = Bot


xor_bit :: (Sbit, Sbit) -> Sbit
xor_bit (a, Szero)      = a
xor_bit (Szero, a)      = a
xor_bit (a, Sone)       = complementSbit a
xor_bit (Sone, a)       = complementSbit a
xor_bit ((B i), (B j)) | i == j    = Szero
                       | i == -j   = Sone
                       | otherwise = Bot
xor_bit (_, Bot)     = Bot
xor_bit (Bot, _)     = Bot

plus_bit :: (Sbit, Sbit) -> (Sword, Sbit) -> (Sword, Sbit)
plus_bit (a, b) (acc, t) = ((xor_bit (xab, t)) : acc, or_bit (oa, abt))
    where xab = xor_bit (a, b)
          aab = and_bit (a, b)
          aat = and_bit (a, t)
          abt = and_bit (b, t)
          oa  = or_bit (aab, aat)

sand :: Sword -> Sword -> Sword
sand a b = map and_bit $ zip a b

sor :: Sword -> Sword -> Sword
sor  a b = map or_bit $ zip a b

sxor :: Sword -> Sword -> Sword
sxor a b = map xor_bit $ zip a b

splus :: Sword -> Sword -> Sword
splus a b = fst . foldr plus_bit ([], Szero) $ zip a b
