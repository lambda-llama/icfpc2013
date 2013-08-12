{-# LANGUAGE BangPatterns #-}

module Language.BV.Simplifier
  ( isNotRedundant
  ) where

import Language.BV.Types
import Language.BV.Symbolic.Types (Sword, slike)
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
isNotRedundant ctx !e =
    not $
    isNotNot e ||
    isShr e ctx ||
    isOp2Zero e ||
    isLogicNotZero e ||
    isWrongOrder e ||
    isSameIfStart e ||
    isDeMorgan e ctx ||
    isDrunk e ctx ||
    isPlusShr e ctx ||
    isClosedFold e ctx ||
    isLogicRepeat e ||
    isTrivialIf e ctx ||
    isSlike e ctx
{-# INLINE isNotRedundant #-}

isNotNot :: BVExpr -> Bool
isNotNot (Op1 Not (Op1 Not _e1)) = True
isNotNot _ =  False
{-# INLINE isNotNot #-}

isShr :: BVExpr -> [String] -> Bool
isShr (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 _)))) = ("shr4" `elem`)
isShr (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 _)))) = ("shr16" `elem`)
isShr _e = const False
{-# INLINE isShr #-}

isPlusShr :: BVExpr -> [String] -> Bool
isPlusShr (Op2 Plus e1 (Op2 Plus e2 _e3)) ctx =
    e1 `like` e2 && "shl1" `elem` ctx
isPlusShr (Op2 Plus e1 e2) ctx                =
    e1 `like` e2 && "shl1" `elem` ctx
isPlusShr _e _ctx = False
{-# INLINE isPlusShr #-}


isTrivialIf :: BVExpr -> [String] -> Bool
isTrivialIf (If0 e0 e1 e2) _ctx =
       e0 `like` e1
    || e1 `like` e2
    || let !res = sevalExprStd e0 in isZero res || isNotZero res
isTrivialIf _e _ctx = False
{-# INLINE isTrivialIf #-}

isOp2Zero :: BVExpr -> Bool
isOp2Zero (Op2 _e0 _e1 Zero)  = True
isOp2Zero (Op2 _op2 Zero _e1) = True
isOp2Zero _e = False
{-# INLINE isOp2Zero #-}

isLogicRepeat :: BVExpr -> Bool
isLogicRepeat (Op2 And e1 e2) = e1 `like` e2
isLogicRepeat (Op2 Or e1 e2)  = e1 `like` e2
isLogicRepeat (Op2 Xor e1 e2) = e1 `like` e2
isLogicRepeat (Op2 op1 e1 (Op2 op2 e2 _)) = op1 == op2 && e1 `like` e2
isLogicRepeat _e = False
{-# INLINE isLogicRepeat #-}

isLogicNotZero :: BVExpr -> Bool
isLogicNotZero (Op2 op2 _e0 (Op1 Not Zero)) = op2 `elem` [And, Or, Xor]
isLogicNotZero (Op2 op2 (Op1 Not Zero) _e3) = op2 `elem` [And, Or, Xor]
isLogicNotZero _e = False
{-# INLINE isLogicNotZero #-}

isWrongOrder :: BVExpr -> Bool
isWrongOrder (Op2 op1 e1 (Op2 op2 e2 _e3)) = op1 == op2 && e1 > e2
isWrongOrder (Op2 _op1 e1 e2)              = e1 > e2
isWrongOrder _e = False
{-# INLINE isWrongOrder #-}

isSameIfStart :: BVExpr -> Bool
isSameIfStart (If0 _e0 (Op1 op1 _e1) (Op1 op2 _e2)) = op1 == op2
isSameIfStart (If0 _e0 (Op2 op1 _e1 _e2) (Op2 op2 _e3 _e4)) = op1 == op2
isSameIfStart _e = False
{-# INLINE isSameIfStart #-}

isDeMorgan :: BVExpr -> [String] -> Bool
isDeMorgan (Op2 Or (Op1 Not _) (Op1 Not _))  = ("and" `elem`)
isDeMorgan (Op2 And (Op1 Not _) (Op1 Not _)) = ("or" `elem`)
isDeMorgan _e = const False
{-# INLINE isDeMorgan #-}

isClosedFold :: BVExpr -> [String] -> Bool
isClosedFold e@(Fold (BVFold _ i (_, _, body))) ctx =
    ('y' `notElem` freeVars body &&
     "shr16" `elem` ctx &&
     "shr4"  `elem` ctx) ||
    isClosed body ||
    e `like` i
isClosedFold _e _ctx = False
{-# INLINE isClosedFold #-}

slikeConsts1 :: [(BVExpr, Sword)]
slikeConsts1 = [(expr, sevalExprStd expr) | expr <- [Zero, One, Id 'x']]

slikeConsts2 :: [(BVExpr, Sword)]
slikeConsts2 = [ (expr, sevalExprStd expr)
               | expr <- [Op1 Not Zero, Op1 Not One, Op1 Not (Id 'x')]
               ]

slikeConsts3 :: [(BVExpr, Sword)]
slikeConsts3 = [(expr, sevalExprStd expr) | expr <- [Op1 Shr16 (Id 'x')]]

slikeConsts4 :: [(BVExpr, Sword)]
slikeConsts4 = [(expr, sevalExprStd expr) | expr <- [Op1 Shr16 (Op1 Not One)]]

isSlike :: BVExpr -> [String] -> Bool
isSlike e ctx =
    f slikeConsts1 ||
    (f slikeConsts2 && ("not" `elem` ctx)) ||
    (f slikeConsts3 && ("shr16" `elem` ctx)) ||
    (f slikeConsts4 && ("not" `elem` ctx) && ("shr16" `elem` ctx))
  where
    f  = any (\(c, s) -> e /= c && slike se s)
    se = sevalExprStd e
{-# INLINE isSlike #-}

isDrunk :: BVExpr -> [String] -> Bool
isDrunk (Op2 And _e0 (Op1 Not One)) ctx = "shr1" `elem` ctx && "shl1" `elem` ctx
isDrunk (Op2 And (Op1 Not One) _e2) ctx = "shr1" `elem` ctx && "shl1" `elem` ctx
isDrunk _e _cts = False
{-# INLINE isDrunk #-}
