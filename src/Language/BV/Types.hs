{-# LANGUAGE RecordWildCards #-}

module Language.BV.Types
  ( BVProgram(..)
  , BVExpr(..)

  , BVId
  , BVFold(..)
  , BVOp1(..)
  , BVOp2(..)
  ) where

import Text.Printf (printf)


type BVId = String

data BVFold = BVFold { bvfArg    :: BVExpr
                     , bvfInit   :: BVExpr
                     , bvfLambda :: (BVId, BVId, BVExpr)
                     }
    deriving (Eq, Ord)

data BVOp1 = Not | Shl1 | Shr1 | Shr4 | Shr16
    deriving (Eq, Ord)

instance Show BVOp1 where
    show Not   = "not"
    show Shl1  = "shl1"
    show Shr1  = "shr1"
    show Shr4  = "shr4"
    show Shr16 = "shr16"

data BVOp2 = And | Or | Xor | Plus
    deriving (Eq, Ord)

instance Show BVOp2 where
    show And  = "and"
    show Or   = "or"
    show Xor  = "xor"
    show Plus = "plus"

data BVExpr = Zero
            | One
            | Id BVId
            | If0 BVExpr BVExpr BVExpr
            | Fold BVFold
            | Op1 BVOp1 BVExpr
            | Op2 BVOp2 BVExpr BVExpr
    deriving (Eq, Ord)


instance Show BVExpr where
    show Zero = "0"
    show One  = "1"
    show (Id bvid) = bvid
    show (If0 econd e1 e2) =
        printf "(if0 %s %s %s)" (show econd) (show e1) (show e2)
    show (Fold (BVFold { bvfLambda = (larg1, larg2, le), .. })) =
        printf "(fold %s %s (lambda (%s %s) %s))"
        (show bvfArg) (show bvfInit) (show larg1) (show larg2) (show le)
    show (Op1 op1 e) = printf "(%s %s)" (show op1) (show e)
    show (Op2 op2 e1 e2) = printf "(%s %s %s)" (show op2) (show e1) (show e2)


newtype BVProgram = BVProgram (BVId, BVExpr)

instance Show BVProgram where
    show (BVProgram (arg, e)) = printf "(lambda (%s) %s)" (show arg) (show e)
