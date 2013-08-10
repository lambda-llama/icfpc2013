{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Symbolic.SEval where

import Language.BV.Symbolic.Types
import Language.BV.Types

seval :: BVExpr -> [(BVId, Sword)] -> Sword
seval e !env = go e where
  go Zero = zero
  go One  = one
  go (Id x) = case lookup x env of
      Nothing -> error (x : " is not defined!")
      Just v  -> v
  go (If0 e0 e1 e2) =
      let !v0 = go e0
          v1  = go e1
          v2  = go e2
      in case (isZero v0, isNotZero v0) of
          (True, _) -> v1
          (_, True) -> v2
          _ -> v1 `merge` v2
  go (Fold _bvf)  = bot
  go (Op1 op1 e0) =
      let !v0 = seval e0 env in
      case op1 of
          Not   -> snot v0
          Shl1  -> sshl1 v0
          Shr1  -> sshr1 v0
          Shr4  -> sshr4 v0
          Shr16 -> sshr16 v0
  go (Op2 op2 e0 e1) =
      let !v0 = seval e0 env
          !v1 = seval e1 env
      in case op2 of
          And  -> v0 `sand` v1
          Or   -> v0 `sor` v1
          Xor  -> v0 `sxor` v1
          Plus -> v0 `splus` v1


zero :: Sword
zero = take 64 $ repeat Szero

one :: Sword
one = (tail zero) ++ [Sone]

bot:: Sword
bot = take 64 $ repeat Bot

isZero :: Sword -> Bool
isZero = (==zero)

isNotZero :: Sword -> Bool
isNotZero = any (==Sone)

input :: Sword
input = [B i | i <- [1..64]]

inputy :: Sword
inputy = [B (65 + i) | i <- [0..63]]

inputz :: Sword
inputz = [B (130 + i) | i <- [0..63]]

stdcontext :: [(BVId, Sword)]
stdcontext = [('x', input), ('y', inputy), ('z', inputz)]

merge :: Sword -> Sword -> Sword
merge a b = map (uncurry lb) (zip a b)

snot :: Sword -> Sword
snot = map comp

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
xor_bit (a, Sone)       = comp a
xor_bit (Sone, a)       = comp a
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
splus a b = fst . foldr plus_bit ([], Szero)$ zip a b

like :: BVExpr -> BVExpr -> Bool
like e0 e1 =
    e0 == e1 ||
    slike (seval e0 stdcontext) (seval e1 stdcontext)
