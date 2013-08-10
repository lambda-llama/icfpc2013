{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Gen where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.BV.Types
import Language.BV.Util
import Language.BV.Simplifier (simplify)
import Language.BV.Symbolic.SEval (sevalExpr, stdContext)
import Language.BV.Symbolic.Types (Sbit(..))

genExpr :: [String] -> Int -> [BVExpr]
genExpr ops =
    let specgen = (m Map.!)
        BVOpTags { .. } = opTagsFromList ops
        m       = Map.fromList [ ((i, flag), undup (go i flag))
                               | i <- [1..30], flag <- [0, 1, 2]
                               ]
        -- go _ 0 -> exprs without fold with x, y, z ids
        -- go _ 1 -> exprs without fold with x ids
        -- go _ 2 -> exprs with fold with x ids
        go :: Int -> Int -> [BVExpr]
        go 1 0     = [Zero, One, Id 'x', Id 'y', Id 'z']
        go 1 _     = [Zero, One, Id 'x']
        go i flag  = concat [op1s, op2s, ifs, folds, tfolds] where
          op1s = [ Op1 op1 x
                 | op1 <- bvOp1s
                 , x   <- specgen (i - 1, flag)
                 ]
          op2s = [ Op2 op2 x y
                 | op2    <- bvOp2s
                 , (j, k) <- partitions2 (pred i)
                 , x <- specgen (j, flag)
                 , y <- specgen (k, flag)
                 , x >= y
                 ]
          ifs  = [ if_ x y z
                 | if_ <- bvIfs
                 , (j, k, l) <- partitions3 (pred i)
                 , x <- specgen (j, flag)
                 , y <- specgen (k, flag)
                 , z <- specgen (l, flag)
                 ]
          (folds, tfolds) = case flag of
              0 -> ([], [])
              1 -> ([], [])
              2 -> ([ f e1 e2 e3
                    | f <- bvFolds
                    , (j, k, l) <- partitions3 (i - 2)
                    , e1 <- specgen (j, 0)
                    , e2 <- specgen (k, 1)
                    , e3 <- specgen (l, 1)
                    ],
                    [ f e1 e2
                    | f <- bvTFolds
                    , (j, k) <- partitions2 (i - 3)
                    , e1 <- specgen (j, 0)
                    , e2 <- specgen (k, 1)
                    ])
              _state -> error "genExpr.go: the impossible happened!"
    in \size -> specgen (size, 2)

undup :: [BVExpr] -> [BVExpr]
undup exprs =
    let simplifiedExprs = Set.toList . Set.fromList $ do
            -- Note(superbobry): we don't distinguish between Left-Right at
            -- the moment.
            expr <- exprs
            return $ case simplify expr of
                Left _e          -> expr
                Right simplified -> simplified
        m = Map.fromListWithKey
            (\k [x] acc -> if any (== Bot) k then x : acc else acc) $
            [(sevalExpr stdContext expr, [expr]) | expr <- simplifiedExprs]
    in Map.foldlWithKey'
       (\mergedExprs k acc ->
         if any (== Bot) k then acc ++ mergedExprs else head acc : mergedExprs)
       [] m
