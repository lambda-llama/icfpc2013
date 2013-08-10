{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Language.NBV.Translator where

import Language.NBV.Types
import qualified Language.BV.Types as BVT

translate :: NBVExpr -> BVT.BVExpr
translate (Id x)            = BVT.Id x
translate Zero              = BVT.Zero
translate One               = BVT.One
translate (Op1 Not e)       = BVT.Op1 BVT.Not (translate e)
translate (Op1 (Shift i) e) | i == 0    = translate e
                            | i > 0     = BVT.Op1 BVT.Shl1  (translate (Op1 (Shift (i - 1)) e))
                            | i <= -16  = BVT.Op1 BVT.Shr16 (translate (Op1 (Shift (i + 16)) e))
                            | i <= -4   = BVT.Op1 BVT.Shr4  (translate (Op1 (Shift (i + 4)) e))
                            | otherwise = BVT.Op1 BVT.Shr1  (translate (Op1 (Shift (i + 1)) e))
translate (OpM And xs)   = foldr1 (\x y -> BVT.Op2 BVT.And  x y) $ map translate xs
translate (OpM Or xs)    = foldr1 (\x y -> BVT.Op2 BVT.Or   x y) $ map translate xs
translate (OpM Xor xs)   = foldr1 (\x y -> BVT.Op2 BVT.Xor  x y) $ map translate xs
translate (OpM Plus xs)  = foldr1 (\x y -> BVT.Op2 BVT.Plus x y) $ map translate xs
translate (If0 e0 e1 e2) = BVT.If0 (translate e0) (translate e1) (translate e2)
translate (Fold (NBVFold f i (y, z, e))) = 
    BVT.Fold (BVT.BVFold (translate f) (translate i) (y, z, translate e))