module Language.BV.Symbolic.Types where

type Sword = [Sbit]

data Sbit = Szero
          | Sone
          | B {-# UNPACK #-} !Int
          | Bot
    deriving (Eq, Show, Ord)

lb :: Sbit -> Sbit -> Sbit
lb a b = if a == b then a else Bot
{-# INLINE lb #-}

slike :: Sword -> Sword -> Bool
slike sw1 sw2 = and $ zipWith eq sw1 sw2 where
  eq Bot _ = False
  eq _ Bot = False
  eq a b   = a == b
{-# INLINE slike #-}
