{-# LANGUAGE BangPatterns #-}

module Language.BV.Symbolic.Operations where

import qualified Data.Vector as V

import Language.BV.Symbolic.Types (Sword, Sbit(..))

zero :: Sword
zero = V.replicate 64 Szero
{-# INLINE zero #-}

one :: Sword
one = V.snoc (V.tail zero) Sone
{-# INLINE one #-}

bot:: Sword
bot = V.replicate 64 Bot
{-# INLINE bot #-}


isZero :: Sword -> Bool
isZero = (== zero)
{-# INLINE isZero #-}

isNotZero :: Sword -> Bool
isNotZero = V.any (== Sone)
{-# INLINE isNotZero #-}

isCombinat :: Sword -> Bool
isCombinat = V.any (== Bot)
{-# INLINE isCombinat #-}

merge :: Sword -> Sword -> Sword
merge = V.zipWith (\a b -> if a == b then a else Bot)
{-# INLINE merge #-}


snot :: Sword -> Sword
snot = V.map complementSbit
{-# INLINE snot #-}

sshl1 :: Sword -> Sword
sshl1 !sw = if V.null sw
            then error "sshl1: the impossible happened!"
            else V.snoc (V.tail sw) Szero
{-# INLINE sshl1 #-}

sshr1 :: Sword -> Sword
sshr1 = V.cons Szero . V.init
{-# INLINE sshr1 #-}

sshr4 :: Sword -> Sword
sshr4 = sshr1 . sshr1 . sshr1 . sshr1
{-# INLINE sshr4 #-}

sshr16 :: Sword -> Sword
sshr16 = sshr4 . sshr4 . sshr4 . sshr4
{-# INLINE sshr16 #-}

sand :: Sword -> Sword -> Sword
sand = V.zipWith andBit
{-# INLINE sand #-}

sor :: Sword -> Sword -> Sword
sor = V.zipWith orBit
{-# INLINE sor #-}

sxor :: Sword -> Sword -> Sword
sxor = V.zipWith xorBit
{-# INLINE sxor #-}

splus :: Sword -> Sword -> Sword
splus a0 b0 = case V.foldr plusBit (V.empty, Szero) $ V.zip a0 b0 of
    (sw, _) -> sw
  where
    plusBit (a, b) (acc, t) = (xorBit xab t `V.cons` acc, orBit oa abt)
      where
        !xab = xorBit a b
        !aab = andBit a b
        !aat = andBit a t
        !abt = andBit b t
        !oa  = orBit aab aat
{-# INLINE splus #-}


complementSbit :: Sbit -> Sbit
complementSbit b = case b of
    Szero -> Sone
    Sone  -> Szero
    B i   -> B (-i)
    Bot   -> Bot
{-# INLINE complementSbit #-}

andBit :: Sbit -> Sbit -> Sbit
andBit _ Szero  = Szero
andBit Szero _  = Szero
andBit a Sone   = a
andBit Sone a   = a
andBit (B i) (B j)
    | i == j    = B i
    | i == -j   = Szero
    | otherwise = Bot
andBit _ _      = Bot
{-# INLINE andBit #-}

orBit :: Sbit -> Sbit -> Sbit
orBit a Szero   = a
orBit Szero a   = a
orBit _ Sone    = Sone
orBit Sone _    = Sone
orBit (B i) (B j)
    | i == j    = B i
    | i == -j   = Sone
    | otherwise = Bot
orBit _ _       = Bot
{-# INLINE orBit #-}

xorBit :: Sbit -> Sbit -> Sbit
xorBit a Szero  = a
xorBit Szero a  = a
xorBit a Sone   = complementSbit a
xorBit Sone a   = complementSbit a
xorBit (B i) (B j)
    | i == j    = Szero
    | i == -j   = Sone
    | otherwise = Bot
xorBit _ _      = Bot
{-# INLINE xorBit #-}
