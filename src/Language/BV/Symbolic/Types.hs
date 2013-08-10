module Language.BV.Symbolic.Types where

import Language.BV.Types

data Sword = [Sbit]
           deriving (Eq, Show)

data Sbit = Zero|One|B Int8|Bot
          deriving (Eq, Show)


liftop1 :: (Integral -> Integral) -> (Sbit -> Sbit)
liftop1 = undefined

liftop2 :: (Integral -> Integral-> Integral) -> (Sbit -> Sbit-> Sbit)
liftop2 = undefined

seval :: BVProgram -> Sword
seval = undefined


