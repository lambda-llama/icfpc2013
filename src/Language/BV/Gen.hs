{-# LANGUAGE RecordWildCards #-}

module Language.BV.Gen where

import Language.BV.Types
import qualified Data.Map as Map

import Language.BV.Util
genexpr :: Int -> [BVExpr]
genexpr =
    let
        m = Map.fromList [(i, go i) | i <- [1..10]]
        go i =
            case i of
                1 -> [Zero, One, Id "x"]
                otherwise -> [Op1 op1 x | op1 <- [Not, Shl1, Shr1, Shr4, Shr16], x <- (genexpr (i - 1))] ++
                             [Op2 op2 x y | op2 <- [And, Or, Xor, Plus], j <- [1..i-1], k <- [1..i-1],
                              x <- (genexpr j), y <- (genexpr k), x >= y, j + k == i - 1] ++
                             [If0 x y z | j <- [1..i-1], k <- [1..i-1], l <- [1..i-1],
                              x <- (genexpr j), y <- (genexpr k), z <- (genexpr l), j + k + l == i - 1]
    in
     \i -> m Map.! i
