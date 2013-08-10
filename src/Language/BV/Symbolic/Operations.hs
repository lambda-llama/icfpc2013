{-# LANGUAGE BangPatterns #-}

module Language.BV.Symbolic.Operations where

import Language.BV.Symbolic.Types

zero :: Sword
zero = take 64 $ repeat Szero
{-# INLINE zero #-}

one :: Sword
one = tail zero ++ [Sone]
{-# INLINE one #-}

bot:: Sword
bot = take 64 $ repeat Bot
{-# INLINE bot #-}


isZero :: Sword -> Bool
isZero = (== zero)

isNotZero :: Sword -> Bool
isNotZero = any (== Sone)


merge :: Sword -> Sword -> Sword
merge a b = map (uncurry lb) (zip a b)


snot :: Sword -> Sword
snot = map complementSbit
{-# INLINE snot #-}

sshl1 :: Sword -> Sword
sshl1 (_bit:sw) = sw ++ [Szero]
sshl1 _sw       = error "sshl1: the impossible happened!"
{-# INLINE sshl1 #-}

sshr1 :: Sword -> Sword
sshr1 !sw = Szero : init sw
{-# INLINE sshr1 #-}

sshr4 :: Sword -> Sword
sshr4 !sw = Szero : Szero : Szero : Szero : take (64 - 4) sw
{-# INLINE sshr4 #-}

sshr16 :: Sword -> Sword
sshr16 !sw = Szero : Szero : Szero : Szero :
            Szero : Szero : Szero : Szero :
            Szero : Szero : Szero : Szero :
            Szero : Szero : Szero : Szero : take (64 - 16) sw
{-# INLINE sshr16 #-}

sand :: Sword -> Sword -> Sword
sand a b = [andBit aa bb | !(aa, bb) <- zip a b]
{-# INLINE sand #-}

sor :: Sword -> Sword -> Sword
sor a b = [orBit aa bb | !(aa, bb) <- zip a b]
{-# INLINE sor #-}

sxor :: Sword -> Sword -> Sword
sxor a b = [xorBit aa bb | !(aa, bb) <- zip a b]
{-# INLINE sxor #-}

splus :: Sword -> Sword -> Sword
splus a0 b0 = case foldr plusBit ([], Szero) $ zip a0 b0 of
    (sw, _) -> sw
  where
    plusBit (a, b) (acc, t) = (xorBit xab t : acc, orBit oa abt)
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
