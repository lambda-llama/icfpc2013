{-# LANGUAGE BangPatterns #-}

module Language.BV.Simplifier
  ( isNotRedundant
  ) where

import Language.BV.Types
import Language.BV.Symbolic.SEval (like, sevalExprStd)
import Language.BV.Symbolic.Operations (isZero, isNotZero)


-- Trnsformations:
--
-- not (not e)             = e
-- (or e 0) = (or 0 e)     = e
-- (and (not e) e)         = 0
-- (or (not e) e)          = (not 0)
-- (and e 0)               = 0
-- (and e e)               = e
-- (or e e)                = e
-- (xor e e)               = 0
-- (plus e 0) = (plus 0 e) = e
-- (shr4 (shr4 (shr4 (shr4 e)))) = (shr16 e)
-- (shr1 (shr1 (shr1 (shr1 e)))) = (shr4 e)
--
-- De'Morgan laws
-- (or (not e1) (not e2))  = (not (and e1 e2))
-- (and (not e1) (not e2)) = (not (or e1 e2))
--
-- Equivalent-if-branches
-- (if e0 e1 e1)           = e1
--
-- Not-0-laws
-- (and e (not 0))         = e
-- (or e (not 0))          = (not 0)
-- (xor e (not 0))         = (not e)
--
-- Multiple-2 law
-- (plus e e)              = (shl1 e)

isNotRedundant :: [String] -> BVExpr -> Bool
isNotRedundant context !e =
    not $
    isNotNot e context ||
    isShr e context ||
    isPlusShr e context ||
    isTrivialIf e context ||
    isOp2Zero e context ||
    isLogicRepeat e context ||
    isLogicNotZero e context || -- good
    isWrongOrder e context ||   -- good
    isDeMorgan e context ||     -- good
    isSameIfStart e context ||  -- good
    isClosedFold e context ||   -- good
    isSlike e context ||        -- good
    isDrunk e context           -- good
{-# INLINE isNotRedundant #-}

isNotNot :: BVExpr -> [String] -> Bool
isNotNot (Op1 Not (Op1 Not _)) _ = True
isNotNot _ _ = False
{-# INLINE isNotNot #-}

isShr :: BVExpr -> [String] -> Bool
isShr (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 _)))) context = "shr4" `elem` context  -- shr4
isShr (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 _)))) context = "shr16" `elem` context -- shr16
isShr _ _ = False
{-# INLINE isShr #-}

isPlusShr :: BVExpr -> [String] -> Bool
isPlusShr (Op2 Plus e1 (Op2 Plus e2 _)) context = e1 `like` e2 && "shl1" `elem` context -- shl1
isPlusShr (Op2 Plus e1 e2) context              = e1 `like` e2 && "shl1" `elem` context -- shl1
isPlusShr _ _ = False
{-# INLINE isPlusShr #-}


isTrivialIf :: BVExpr -> [String] -> Bool
isTrivialIf (If0 e0 e1 e2) context =
    e0 `like` e1
    || e1 `like` e2
    ||  let !res = sevalExprStd e0
        in isZero res || isNotZero res
isTrivialIf _ _ = False
{-# INLINE isTrivialIf #-}

isOp2Zero :: BVExpr -> [String] -> Bool
isOp2Zero (Op2 _ _ Zero) _  = True
isOp2Zero (Op2 _ Zero _) _  = True
isOp2Zero _ _ = False
{-# INLINE isOp2Zero #-}

isLogicRepeat :: BVExpr -> [String] -> Bool
isLogicRepeat (Op2 And e1 e2) _ = e1 `like` e2
isLogicRepeat (Op2 Or e1 e2) _  = e1 `like` e2
isLogicRepeat (Op2 Xor e1 e2) _ = e1 `like` e2
isLogicRepeat (Op2 op1 e1 (Op2 op2 e2 _)) _ = op1 == op2 && e1 `like` e2
isLogicRepeat _ _ = False
{-# INLINE isLogicRepeat #-}

isLogicNotZero :: BVExpr -> [String] -> Bool
isLogicNotZero (Op2 _ _ (Op1 Not Zero)) _ = True
isLogicNotZero (Op2 _ (Op1 Not Zero) _) _ = True
isLogicNotZero _ _ = False
{-# INLINE isLogicNotZero #-}

isWrongOrder :: BVExpr -> [String] -> Bool
isWrongOrder (Op2 op1 e1 (Op2 op2 e2 _)) _ = op1 == op2 && e1 > e2
isWrongOrder (Op2 _ e1 e2) _               = e1 > e2
isWrongOrder _ _ = False
{-# INLINE isWrongOrder #-}

isSameIfStart :: BVExpr -> [String] -> Bool
isSameIfStart (If0 _ (Op1 op1 _) (Op1 op2 _)) _     = op1 == op2
isSameIfStart (If0 _ (Op2 op1 _ _) (Op2 op2 _ _)) _ = op1 == op2
isSameIfStart _ _ = False
{-# INLINE isSameIfStart #-}

isDeMorgan :: BVExpr -> [String] -> Bool
isDeMorgan (Op2 Or (Op1 Not _) (Op1 Not _)) context  = "and" `elem` context -- and
isDeMorgan (Op2 And (Op1 Not _) (Op1 Not _)) context = "or" `elem` context -- or
isDeMorgan _ _ = False
{-# INLINE isDeMorgan #-}

isClosedFold :: BVExpr -> [String] -> Bool
isClosedFold e@(Fold (BVFold _ i (_, _, body))) context = ('y' `notElem` freeVars body &&
                                                           "shr16" `elem` context && "shr4" `elem` context) ||
                                                          isClosed body ||
                                                          e `like` i
isClosedFold _ _ = False
{-# INLINE isClosedFold #-}

isSlike :: BVExpr -> [String] -> Bool
isSlike e context = (f [Zero, One, Id 'x']) ||
                    ((f [Op1 Not Zero, Op1 Not One, Op1 Not (Id 'x')]) && ("not" `elem` context)) ||
                    ((f [Op1 Shr16 (Id 'x')]) && ("shr16" `elem` context)) ||
                    ((f [Op1 Shr16 (Op1 Not One)]) && ("not" `elem` context) && ("shr16" `elem` context)) -- not shr16
  where f = any (\c -> e /= c && like e c)

isDrunk :: BVExpr -> [String] -> Bool
isDrunk (Op2 And _ (Op1 Not One)) context = ("shr1" `elem` context) && ("shl1" `elem` context) -- shr1 shl1
isDrunk (Op2 And (Op1 Not One) _) context = ("shr1" `elem` context) && ("shl1" `elem` context) -- shr1 shl1
isDrunk _ _ = False
{-# INLINE isSlike #-}
