module Language.BV.Symbolic.Types where

import Data.Int(Int8)

import Language.BV.Types

type Sword = [Sbit]
           
data Sbit = Zero|One|B Int8|Bot
          deriving (Eq, Show, Ord)
