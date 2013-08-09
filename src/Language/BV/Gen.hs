{-# LANGUAGE RecordWildCards #-}

module Language.BV.Gen where

import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import Language.BV.Types
import Language.BV.Util

genExpr :: [String] -> Int -> [BVExpr]
genExpr ops =
    let
        specgen = \size -> m Map.! size
        op1s = catMaybes $ map op1ByTag ops
        op2s = catMaybes $ map op2ByTag ops
        ifs = catMaybes $ map ifByTag ops
        m = Map.fromList [(i, go i) | i <- [1..42]]
        go 1 = [Zero, One, Id "x"]
        go i = [ Op1 op1 x
               | op1 <- op1s
               , x   <- specgen (i - 1)
               ] ++
               [ Op2 op2 x y
               | op2 <- op2s
               , j  <- [1..i-1]
               , k  <- [1..i-1]
               , x  <- specgen j
               , y  <- specgen k
               , x >= y, j + k == i - 1
               ] ++
               [ if_ x y z
               | if_ <- ifs
               , j <- [1..i-1]
               , k <- [1..i-1]
               , l <- [1..i-1]
               , x <- specgen j
               , y <- specgen k
               , z <- specgen l
               , j + k + l == i - 1
               ]
    in \size -> m Map.! size


countExpr:: [String] -> Int -> Int
countExpr ops =
    let
        speccount = countExpr ops
        op1s = length . catMaybes $ map op1ByTag ops
        op2s = length . catMaybes $ map op2ByTag ops
        ifs = length . catMaybes $ map ifByTag ops
        m = Map.fromList [(i, go i) | i <- [1..42]]
        go 1  = 3
        go i = op1s * speccount (i - 1)
                + op2s  * sum [ x * y
                             | j  <- [1..i-1]
                             , k  <- [1..i-1]
                             , x  <- [speccount j]
                             , y  <- [speccount k]
                             , j + k == i - 1
                             ]
                + ifs * sum [ x * y * z
                            | j <- [1..i-1]
                            , k <- [1..i-1]
                            , l <- [1..i-1]
                            , x <- [speccount j]
                            , y <- [speccount k]
                            , z <- [speccount l]
                            , j + k + l == i - 1
                            ]
    in \size -> m Map.! size 
