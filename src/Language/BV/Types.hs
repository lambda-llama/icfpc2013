{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.BV.Types
  ( BVProgram(..)
  , BVExpr(..)
  , isClosed

  , BVId
  , BVFold(..)
  , BVOp1(..)
  , BVOp2(..)

  , BVOpTags(..)
  , opTagsFromList

  , operators
  , freeVars
  ) where

import Data.List ((\\))
import Data.Hashable (Hashable(..))
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import qualified Data.Map as Map


type BVId = Char

data BVFold = BVFold { bvfArg    :: !BVExpr
                     , bvfInit   :: !BVExpr
                     , bvfLambda :: !(BVId, BVId, BVExpr)
                     }
    deriving (Eq, Ord)

instance Hashable BVFold where
    hashWithSalt salt (BVFold { .. }) =
        hashWithSalt salt (bvfInit, bvfArg, bvfLambda)
    {-# INLINE hashWithSalt #-}

data BVOp1 = Not | Shl1 | Shr1 | Shr4 | Shr16
    deriving (Eq, Ord, Enum, Bounded)

instance Show BVOp1 where
    show Not   = "not"
    show Shl1  = "shl1"
    show Shr1  = "shr1"
    show Shr4  = "shr4"
    show Shr16 = "shr16"

instance Hashable BVOp1 where
    hashWithSalt salt = hashWithSalt salt . fromEnum
    {-# INLINE hashWithSalt #-}

data BVOp2 = And | Or | Xor | Plus
    deriving (Eq, Ord, Enum, Bounded)

instance Hashable BVOp2 where
    hashWithSalt salt = hashWithSalt salt . fromEnum
    {-# INLINE hashWithSalt #-}

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
    show (Id bvid) = [bvid]
    show (If0 e0 e1 e2) =
        printf "(if0 %s %s %s)" (show e0) (show e1) (show e2)
    show (Fold (BVFold { bvfLambda = (larg0, larg1, le0), .. })) =
        printf "(fold %s %s (lambda (%c %c) %s))"
        (show bvfArg) (show bvfInit) larg0 larg1 (show le0)
    show (Op1 op1 e0) = printf "(%s %s)" (show op1) (show e0)
    show (Op2 op2 e0 e1) = printf "(%s %s %s)" (show op2) (show e0) (show e1)

instance Hashable BVExpr where
    hashWithSalt salt = go where
      go Zero      = hashWithSalt salt (0 :: Int)
      go One       = hashWithSalt salt (1 :: Int)
      go (Id bvid) = hashWithSalt salt bvid
      go (If0 e0 e1 e2)  = hashWithSalt salt (e0, e1, e2)
      go (Fold bvf)      = hashWithSalt salt bvf
      go (Op1 op1 e0)    = hashWithSalt salt (op1, e0)
      go (Op2 op2 e0 e1) = hashWithSalt salt (op2, e0, e1)
    {-# INLINE hashWithSalt #-}

isClosed :: BVExpr -> Bool
isClosed Zero = True
isClosed One  = True
isClosed (Id _id) = False
isClosed (If0 e0 e1 e2)  = isClosed e0 && isClosed e1 && isClosed e2
isClosed (Fold (BVFold { bvfLambda = (_larg0, _larg1, le) })) = isClosed le
isClosed (Op1 _op e0)    = isClosed e0
isClosed (Op2 _op e0 e1) = isClosed e0 && isClosed e1
{-# INLINE isClosed #-}

newtype BVProgram = BVProgram (BVId, BVExpr)

instance Show BVProgram where
    show (BVProgram (arg, e)) = printf "(lambda (%c) %s)" arg (show e)


enumFromShow :: (Show a, Enum a, Bounded a) => String -> Maybe a
enumFromShow =
    let !m = Map.fromList [(show op, op) | op <- [minBound..]]
    in (`Map.lookup` m)
{-# SPECIALIZE INLINE enumFromShow :: String -> Maybe BVOp1 #-}
{-# SPECIALIZE INLINE enumFromShow :: String -> Maybe BVOp2 #-}

data BVOpTags = BVOpTags { bvOp1s   :: ![BVOp1]
                         , bvOp2s   :: ![BVOp2]
                         , bvIfs    :: ![BVExpr -> BVExpr -> BVExpr -> BVExpr]
                         , bvFolds  :: ![BVExpr -> BVExpr -> BVExpr -> BVExpr]
                         , bvTFolds :: ![BVExpr -> BVExpr -> BVExpr]
                         }

operators :: [String]
operators = [ "tfold", "fold", "not", "shl1", "shr1"
            , "shr4", "shr16", "and", "or", "xor"
            , "plus", "if0"
            ]


opTagsFromList :: [String] -> BVOpTags
opTagsFromList ops = BVOpTags { .. } where
  bvOp1s   = mapMaybe enumFromShow ops
  bvOp2s   = mapMaybe enumFromShow ops
  bvIfs    = [If0 | "if0" `elem` ops]
  bvFolds  = [mkLambda | "fold" `elem` ops]
  bvTFolds = [\e0 e1 -> mkLambda e0 e1 Zero | "tfold" `elem` ops]

  mkLambda le bvfArg bvfInit = Fold BVFold { bvfLambda = ('y', 'z', le), .. }
{-# INLINE opTagsFromList #-}

freeVars :: BVExpr -> [BVId]
freeVars Zero = []
freeVars One  = []
freeVars (Id x) = [x]
freeVars (Op1 _ e)      = freeVars e
freeVars (Op2 _ e1 e2)  = (freeVars e1) ++ (freeVars e2)
freeVars (If0 e0 e1 e2) = (freeVars e0) ++ (freeVars e1) ++ (freeVars e2)
freeVars (Fold (BVFold f i (y, z, body))) = (freeVars f) ++ (freeVars i) ++ (freeVars body \\ [y, z])
