module Language.BV.Simplifier
  ( simplify
  ) where

import Language.BV.Types

simplify :: BVExpr -> Either BVExpr BVExpr
simplify (Op1 Not (Op1 Not e)) = Right e
simplify e = Left e
