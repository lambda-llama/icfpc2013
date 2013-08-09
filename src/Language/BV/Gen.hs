{-# LANGUAGE RecordWildCards #-}

module Language.BV.Gen where

import Language.BV.Types
import Language.BV.Util

genexprsize :: Int -> [BVExpr]
genexprsize i = case i of
  1         -> 3
  otherwise -> sum [1 + genexprsize (i - 1),
  				1 + (genexprsize (i - 1)) * (genexprsize (i - 1)) / 2,
  			    1 + (genexprsize (i - 1)) * (genexprsize (i - 1)) * (genexprsize (i - 1)),
  			    2 + (genexprsize (i - 1)) * (genexprsize (i - 1)) * (genexprsize (i - 1))]