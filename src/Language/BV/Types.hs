module Language.BV.Types
  ( BVProgram(..)
  , BVExpr(..)

  , BVId
  , BVFold(..)
  , BVOp1(..)
  , BVOp2(..)
  ) where


type BVId = String

data BVFold = BVFold { bvfArg    :: BVExpr
                     , bvfInit   :: BVExpr
                     , bvfLambda :: (BVId, BVId, BVExpr)
                     }
    deriving (Eq, Ord)

data BVOp1 = Not | Shl1 | Shr1 | Shr4 | Shr16
    deriving (Eq, Ord)

data BVOp2 = And | Or | Xor | Plus
    deriving (Eq, Ord)

data BVExpr = Zero
            | One
            | Id BVId
            | If0 BVExpr BVExpr BVExpr
            | Fold BVFold
            | Op1 BVOp1 BVExpr
            | Op2 BVOp2 BVExpr BVExpr
    deriving (Eq, Ord)

newtype BVProgram = BVProgram (BVId, BVExpr)
