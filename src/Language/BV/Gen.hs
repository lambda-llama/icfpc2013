{-# LANGUAGE RecordWildCards #-}

module Language.BV.Gen where

import Language.BV.Types
import qualified Data.Map as Map
import Data.Maybe(catMaybes)

import Language.BV.Util
genexpr :: [String] -> Int -> [BVExpr]
genexpr ops =
    let
        specgen = genexpr ops
        op1s = catMaybes $ map op1ByTag ops
        op2s = catMaybes $ map op2ByTag ops
        ifs = catMaybes $ map ifByTag ops
        m = Map.fromList [(i, go i) | i <- [1..10]]
        go i =
            case i of
                1 -> [Zero, One, Id "x"]
                otherwise -> [Op1 op1 x | op1 <- op1s, x <- specgen(i - 1)] ++
                             [Op2 op2 x y | op2 <- op2s, j <- [1..i-1], k <- [1..i-1],
                              x <- (specgen j), y <- (specgen k), x >= y, j + k == i - 1] ++
                             [if_ x y z | if_ <- ifs, j <- [1..i-1], k <- [1..i-1], l <- [1..i-1],
                              x <- (specgen j), y <- (specgen k), z <- (specgen l), j + k + l == i - 1]
    in
     \i -> m Map.! i
