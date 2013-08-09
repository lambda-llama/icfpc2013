{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Language.BV.Util
  ( exprSize
  , programSize
  ) where

import Language.BV.Types (BVProgram(..), BVExpr(..), BVFold(..))


exprSize :: BVExpr -> Int
exprSize !e = go e where
  go :: BVExpr -> Int
  go (If0 e0 e1 e2) = 1 + go e0 + go e1 + go e2
  go (Fold (BVFold { bvfLambda = (_larg0, _larg1, le0), ..})) =
      2 + go bvfArg + go bvfInit + go le0
  go (Op1 _op1 e0)    = 1 + go e0
  go (Op2 _op2 e0 e1) = 1 + go e0 + go e1
  go _e = 1

programSize :: BVProgram -> Int
programSize (BVProgram (_arg, e)) = 1 + exprSize e
