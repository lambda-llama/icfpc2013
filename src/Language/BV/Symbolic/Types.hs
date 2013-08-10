module Language.BV.Symbolic.Types where

import Data.Int(Int8)

type Sword = [Sbit]
           
data Sbit = Szero|Sone|B Int8|Bot
          deriving (Eq, Show, Ord)
