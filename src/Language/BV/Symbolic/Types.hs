module Language.BV.Symbolic.Types where

import Data.Word(Word64)
import Data.Bits(testBit)

type Sword = [Sbit]

data Sbit = Szero |Sone |B {-# UNPACK #-} !Int |Bot
    deriving (Eq, Show, Ord)

lb :: Sbit -> Sbit -> Sbit
lb a b = case (a, b) of
    _ | a == b -> a
    _ -> Bot

comp :: Sbit -> Sbit
comp a = case a of
    Szero -> Sone
    Sone -> Szero
    B i -> B (-i)
    Bot -> Bot

word2sword :: Word64 -> Sword
word2sword w = [if testBit w i then Sone else Szero |i <- [63,62..0]]

slike :: Sword -> Sword -> Bool
slike sw1 sw2 = all eq $ zip sw1 sw2
    where
        eq (Bot, _) = False
        eq (_, Bot) = False
        eq (a, b)   = a == b
