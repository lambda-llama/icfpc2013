{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Types
  ( BVProgram(..)
  , BVExpr(..)

  , BVId
  , BVFold(..)
  , BVOp1(..)
  , BVOp2(..)

  , enumFromShow
  , ifByTag
  , foldByTag
  , tfoldByTag
  , lambda2
  ) where

import Text.Printf (printf)
import qualified Data.Map as Map


type BVId = String

data BVFold = BVFold { bvfArg    :: !BVExpr
                     , bvfInit   :: !BVExpr
                     , bvfLambda :: !(BVId, BVId, BVExpr)
                     }
    deriving (Eq, Ord)

data BVOp1 = Not | Shl1 | Shr1 | Shr4 | Shr16
    deriving (Eq, Ord, Enum, Bounded)

instance Show BVOp1 where
    show Not   = "not"
    show Shl1  = "shl1"
    show Shr1  = "shr1"
    show Shr4  = "shr4"
    show Shr16 = "shr16"

data BVOp2 = And | Or | Xor | Plus
    deriving (Eq, Ord, Enum, Bounded)

instance Show BVOp2 where
    show And  = "and"
    show Or   = "or"
    show Xor  = "xor"
    show Plus = "plus"

data BVExpr = Zero
            | One
            | Id !BVId
            | If0 !BVExpr !BVExpr !BVExpr
            | Fold !BVFold
            | Op1 !BVOp1 !BVExpr
            | Op2 !BVOp2 !BVExpr !BVExpr
    deriving (Eq, Ord)


instance Show BVExpr where
    show Zero = "0"
    show One  = "1"
    show (Id bvid) = bvid
    show (If0 e0 e1 e2) =
        printf "(if0 %s %s %s)" (show e0) (show e1) (show e2)
    show (Fold (BVFold { bvfLambda = (larg0, larg1, le0), .. })) =
        printf "(fold %s %s (lambda (%s %s) %s))"
        (show bvfArg) (show bvfInit) larg0 larg1 (show le0)
    show (Op1 op1 e0) = printf "(%s %s)" (show op1) (show e0)
    show (Op2 op2 e0 e1) = printf "(%s %s %s)" (show op2) (show e0) (show e1)


newtype BVProgram = BVProgram (BVId, BVExpr)

instance Show BVProgram where
    show (BVProgram (arg, e)) = printf "(lambda (%s) %s)" arg (show e)

enumFromShow :: (Show a, Enum a, Bounded a) => String -> a
enumFromShow =
    let !m = Map.fromList [(show op, op) | op <- [minBound..]]
    in (m Map.!)
{-# SPECIALIZE INLINE enumFromShow :: String -> BVOp1 #-}
{-# SPECIALIZE INLINE enumFromShow :: String -> BVOp2 #-}

-- Note(matklad): this is if0 type V
ifByTag :: String -> Maybe (BVExpr -> BVExpr -> BVExpr -> BVExpr)
ifByTag s = if s == "if0" then Just If0 else Nothing

foldByTag :: String -> Maybe (BVExpr -> BVExpr -> BVExpr -> BVExpr)
foldByTag s = if s == "fold"
              then Just lambda2
              else Nothing

tfoldByTag :: String -> Maybe (BVExpr -> BVExpr -> BVExpr)
tfoldByTag s = if s == "tfold"
               then Just $ \e1 e2 -> lambda2 e1 e2 Zero
               else Nothing

lambda2 :: BVExpr -> BVExpr -> BVExpr -> BVExpr
lambda2 e1 bvfArg bvfInit = Fold $ BVFold { bvfLambda = ("y", "z", e1), .. }
{-# INLINE lambda2 #-}
