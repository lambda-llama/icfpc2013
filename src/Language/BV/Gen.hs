{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Gen where

import Data.Map (Map)
import qualified Data.Map as Map

import Language.BV.Types
import Language.BV.Util
import Language.BV.Simplifier (isNotRedundant)

type State = Map Int [BVExpr]

genExpr :: [String] -> Int -> [BVExpr]
genExpr ops size = concat $ Map.elems r3 where
    BVOpTags { .. } = opTagsFromList ops

    (_r1, _r2, r3) = go size undefined

    -- 1 -> exprs without fold with x, y, z ids
    -- 2 -> exprs without fold with x ids
    -- 3 -> exprs with fold with x ids
    go :: Int -> (State, State, State) -> (State, State, State)
    go 1 _states =
        (Map.singleton 1 [Zero, One, Id 'x', Id 'y', Id 'z'],
         Map.singleton 1 [Zero, One, Id 'x'],
         Map.singleton 1 [Zero, One, Id 'x'])
    go !i states =
        let (!s1, !s2, !s3) = go (pred i) states
            go0 m = filter isNotRedundant $ concat [op1s, op2s, ifs] where
              op1s = [ Op1 op1 x
                     | op1 <- bvOp1s
                     , x   <- m Map.! pred i
                     ]
              op2s = [ Op2 op2 x y
                     | op2    <- bvOp2s
                     , (j, k) <- partitions2 (pred i)
                     , x <- m Map.! j
                     , y <- m Map.! k
                     ]
              ifs  = [ if_ x y z
                     | if_ <- bvIfs
                     , (j, k, l) <- partitions3 (pred i)
                     , x <- m Map.! j
                     , y <- m Map.! k
                     , z <- m Map.! l
                     ]
            go1 = go0 s1
            go2 = go0 s2
            go3 = filter isNotRedundant $
                  go0 s3 ++
                  [ f e1 e2 e3
                  | f <- bvFolds
                  , (j, k, l) <- partitions3 (i - 2)
                  , e1 <- s1 Map.! j
                  , e2 <- s2 Map.! k
                  , e3 <- s2 Map.! l
                  ] ++
                  [ f e1 e2
                  | f <- bvTFolds
                  , (j, k) <- partitions2 (i - 3)
                  , e1 <- s1 Map.! j
                  , e2 <- s2 Map.! k
                  ]
        in (Map.insert i go1 s1,
            Map.insert i go2 s2,
            Map.insert i go3 s3)
{-# INLINEABLE genExpr #-}
