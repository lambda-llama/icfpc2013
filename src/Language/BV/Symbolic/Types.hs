module Language.BV.Symbolic.Types where

import Data.Int(Int8)

import Language.BV.Types

type Sword = [Sbit]
           
data Sbit = Zero|One|B Int8|Bot
          deriving (Eq, Show, Ord)


liftop1 :: (a -> a) -> (Sbit -> Sbit)
liftop1 = undefined

liftop2 :: (a -> a -> a) -> (Sbit -> Sbit-> Sbit)
liftop2 = undefined

seval :: BVProgram -> Sword
seval = undefined


not   = undefined
shl1  = undefined
shr1  = undefined
shr4  = undefined
shr16 = undefined
    
and  = undefined
or   = undefined
xor  = undefined
plus = undefined
