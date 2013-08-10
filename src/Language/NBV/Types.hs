{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.NBV.Types
  ( NBVProgram(..)
  , NBVExpr(..)

  , NBVId
  , NBVFold(..)
  , NBVOp1(..)
  , NBVOpM(..)
 )
  where

import Data.Word (Word64)
import Data.Maybe (mapMaybe)

import Text.Printf (printf)

type NBVId = Char

type NBVContext = [(NBVId, Word64)]

data NBVOp1 = Not | Shift Int
    deriving (Eq, Ord)

instance Show NBVOp1 where
    show Not   = "not"
    show (Shift i)  = "shift " ++ show i

data NBVOpM = And | Or | Xor | Plus
    deriving (Eq, Ord)

instance Show NBVOpM where
    show And  = "and"
    show Or   = "or"
    show Xor  = "xor"
    show Plus = "plus"

data NBVFold = NBVFold { nbvfArg    :: NBVExpr
                       , nbvfInit   :: NBVExpr
                       , nbvfLambda :: (NBVId, NBVId, NBVExpr)
                       }
    deriving (Eq, Ord)

data NBVExpr = Zero
             | One
             | Id NBVId
             | Op1 NBVOp1 NBVExpr
             | OpM NBVOpM [NBVExpr]
             | If0 NBVExpr NBVExpr NBVExpr
             | Fold NBVFold
    deriving (Eq, Ord)

instance Show NBVExpr where
    show Zero       = "0"
    show One        = "1"
    show (Id x)     = [x]
    show (If0 e0 e1 e2) =
        printf "(if0 %s %s %s)" (show e0) (show e1) (show e2)
    show (Fold (NBVFold { nbvfLambda = (larg0, larg1, le0), .. })) =
        printf "(fold %s %s (lambda (%c %c) %s))"
        (show nbvfArg) (show nbvfInit) larg0 larg1 (show le0)
    show (Op1 op e) = printf ("%s %s") (show op) (show e)
    show (OpM op x) = printf ("%s %s") (show op) (show x)

newtype NBVProgram = NBVProgram (NBVId, NBVExpr)

instance Show NBVProgram where
    show (NBVProgram (arg, e)) = printf "(lambda (%c) %s)" arg (show e)