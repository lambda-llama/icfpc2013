module Language.BV.Symbolic.Types where

import Data.Int(Int8)

type Sword = [Sbit]
           
data Sbit = Szero|Sone|B Int8|Bot
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
