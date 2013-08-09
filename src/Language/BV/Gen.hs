{-# LANGUAGE RecordWildCards #-}

module Language.BV.Gen where

import Data.Maybe (mapMaybe)
import Data.Either (lefts, rights)
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Language.BV.Types
import Language.BV.Util
import Language.BV.Simplifier (simplify)

genExpr :: [String] -> Int -> [BVExpr]
genExpr ops =
    let specgen = \(size, f) -> m Map.! (size, f)
        op1s    = mapMaybe op1ByTag ops
        op2s    = mapMaybe op2ByTag ops
        ifs     = mapMaybe ifByTag ops
        folds   = mapMaybe foldByTag ops
        tfolds  = mapMaybe tfoldByTag ops
        m       = Map.fromList [ ((i, f), undup (go i f))
                               | i <- [1..42], f <- [0, 1, 2]
                               ]
        -- go _ 0 -> exprs without fold with x, y, z ids
        -- go _ 1 -> exprs without fold with x ids
        -- go _ 2 -> exprs with fold with x ids
        go :: Int -> Int -> [BVExpr]
        go 1 0  = [Zero, One, Id "x", Id "y", Id "z"]
        go 1 _  = [Zero, One, Id "x"]
        go i f  = [ Op1 op1 x
                  | op1 <- op1s
                  , x   <- specgen (i - 1, f)
                  ] ++
                  [ Op2 op2 x y
                  | op2    <- op2s
                  , (j, k) <- partitions2 (pred i)
                  , x <- specgen (j, f)
                  , y <- specgen (k, f)
                  , x >= y
                  ] ++
                  [ if_ x y z
                  | if_ <- ifs
                  , (j, k, l) <- partitions3 (pred i)
                  , x <- specgen (j, f)
                  , y <- specgen (k, f)
                  , z <- specgen (l, f)
                  ] ++ case f of
                      0 -> []
                      1 -> []
                      2 -> [ f e1 e2 e3
                           | f <- folds
                           , (j, k, l) <- partitions3 (i - 2)
                           , e1 <- specgen (j, 0)
                           , e2 <- specgen (k, 1)
                           , e3 <- specgen (l, 1)
                           ] ++
                           [ f e1 e2
                           | f <- tfolds
                           , (j, k) <- partitions2 (i - 3)
                           , e1 <- specgen (j, 0)
                           , e2 <- specgen (k, 1)
                           ]
    in \size -> specgen (size, 2)

countExpr:: [String] -> Int -> Int
countExpr ops =
    let speccount = countExpr ops
        op1s      = length $ mapMaybe op1ByTag ops
        op2s      = length $ mapMaybe op2ByTag ops
        ifs       = length $ mapMaybe ifByTag ops
        m    = IntMap.fromList [(i, go i) | i <- [1..42]]
        go 1 = 3
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
    in \size -> m IntMap.! size

undup :: [BVExpr] -> [BVExpr]
undup exprs =
    let simplified = map simplify exprs
        ls = Set.fromList $ lefts simplified
        rs = Set.fromList $ rights simplified
    in Set.toList $ ls `Set.union` rs
