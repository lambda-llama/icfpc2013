{-# LANGUAGE RecordWildCards #-}

module Language.BV.Gen where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Language.BV.Types
import Language.BV.Util
import Language.BV.Simplifier (simplify)

genExpr :: [String] -> Int -> [BVExpr]
genExpr ops =
    let specgen = \(size, f) -> m Map.! (size, f)
        BVOpTags { .. } = opTagsFromList ops
        m       = Map.fromList [ ((i, f), undup (go i f))
                               | i <- [1..42], f <- [0, 1, 2]
                               ]
        -- go _ 0 -> exprs without fold with x, y, z ids
        -- go _ 1 -> exprs without fold with x ids
        -- go _ 2 -> exprs with fold with x ids
        go :: Int -> Int -> [BVExpr]
        go 1 0     = [Zero, One, Id 'x', Id 'y', Id 'z']
        go 1 _     = [Zero, One, Id 'x']
        go i flag  =
            [ Op1 op1 x
            | op1 <- bvOp1s
            , x   <- specgen (i - 1, flag)
            ] ++
            [ Op2 op2 x y
            | op2    <- bvOp2s
            , (j, k) <- partitions2 (pred i)
            , x <- specgen (j, flag)
            , y <- specgen (k, flag)
            , x >= y
            ] ++
            [ if_ x y z
            | if_ <- bvIfs
            , (j, k, l) <- partitions3 (pred i)
            , x <- specgen (j, flag)
            , y <- specgen (k, flag)
            , z <- specgen (l, flag)
            ] ++ case flag of
                0 -> []
                1 -> []
                2 -> [ f e1 e2 e3
                     | f <- bvFolds
                     , (j, k, l) <- partitions3 (i - 2)
                     , e1 <- specgen (j, 0)
                     , e2 <- specgen (k, 1)
                     , e3 <- specgen (l, 1)
                     ] ++
                     [ f e1 e2
                     | f <- bvTFolds
                     , (j, k) <- partitions2 (i - 3)
                     , e1 <- specgen (j, 0)
                     , e2 <- specgen (k, 1)
                     ]
    in \size -> specgen (size, 2)

countExpr:: [String] -> Int -> Int
countExpr ops =
    let speccount = countExpr ops
        BVOpTags { .. } = opTagsFromList ops
        m    = IntMap.fromList [(i, go i) | i <- [1..42]]
        go 1 = 3
        go i = length bvOp1s * speccount (i - 1) +
               length bvOp2s * sum [ x * y
                                   | j  <- [1..i-1]
                                   , k  <- [1..i-1]
                                   , x  <- [speccount j]
                                   , y  <- [speccount k]
                                   , j + k == i - 1
                                   ] +
               length bvIfs * sum [ x * y * z
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
undup exprs = do
    -- Note(superbobry): we don't distinguish between Left-Right at
    -- the moment.
    expr <- exprs
    return $ case simplify expr of
        Left _e          -> expr
        Right simplified -> simplified
