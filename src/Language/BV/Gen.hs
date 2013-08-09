{-# LANGUAGE RecordWildCards #-}

module Language.BV.Gen where

import Language.BV.Types

genexpr :: Int -> [BVExpr]
genexpr i = case i of
  1         -> [Zero, One, Id "x"]
  otherwise -> [Op1 op1 x | op1 <- [Not, Shl1, Shr1, Shr4, Shr16], x <- (genexpr (i - 1))] ++
               [Op2 op2 x y | op2 <- [And, Or, Xor, Plus], x <- (genexpr (i - 1)), y <- (genexpr (i - 1)), x >= y] ++
               [If0 x y z | x <- (genexpr (i - 1)), y <- (genexpr (i - 1)), z <- (genexpr (i - 1))]