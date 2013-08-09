{-# LANGUAGE RecordWildCards #-}

module Language.BV.Types
  ( BVProgram(..)
  , BVExpr(..)

  , BVId
  , BVFold(..)
  , BVOp1(..)
  , BVOp2(..)
  , BVOpTag(..)

  , op1ByTag
  , op2ByTag
  , ifByTag
  , foldByTag
  , tfoldByTag
  ) where

import Text.Printf (printf)
import qualified Data.Map as Map


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

data BVOpTag = Op1Tag BVOp1
             | Op2Tag BVOp2
             | If0Tag
             | TFoldTag
             | FoldTag
    deriving (Eq, Ord)

instance Show BVOpTag where
    show (Op1Tag op1) = show op1
    show (Op2Tag op2) = show op2
    show If0Tag       = "if0"
    show TFoldTag     = "tfold"
    show FoldTag      = "fold"

op1ByTag :: String -> Maybe BVOp1
op1ByTag s = let m = Map.fromList [("not", Not)
                                  , ("shl1", Shl1)
                                  , ("shr1", Shr1)
                                  , ("shr4", Shr4)
                                  , ("shr16", Shr16)]
             in
              Map.lookup s m

op2ByTag :: String -> Maybe BVOp2
op2ByTag s = let m = Map.fromList [("and", And)
                                  ,("or", Or)
                                  ,("xor", Xor)
                                  ,("plus", Plus)]
            in
             Map.lookup s m

-- Note(matklas): this is if0 type V
ifByTag :: String -> Maybe (BVExpr -> BVExpr -> BVExpr -> BVExpr) 
ifByTag s = if s == "if0" then Just If0 else Nothing

foldByTag :: String -> Maybe (BVExpr -> BVExpr -> BVExpr -> BVExpr)
foldByTag s = if s == "fold"
              then (Just $ \e1 bvfArg bvfInit ->
                                    Fold $ BVFold {bvfLambda=("y", "z", e1), ..})
              else Nothing

tfoldByTag :: String -> Maybe (BVExpr -> BVExpr -> BVExpr)
tfoldByTag s = if s == "tfold"
               then (Just $ \e1 bvfInit ->
                      Fold $ BVFold {bvfLambda=("y", "z", e1), bvfArg=Zero, ..})
               else Nothing
