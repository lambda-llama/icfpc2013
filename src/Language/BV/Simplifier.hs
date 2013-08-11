module Language.BV.Simplifier
  ( isNotRedundant
  ) where

import Language.BV.Eval (evalExpr)

import Language.BV.Types
import Language.BV.Symbolic.SEval
--import Language.BV.Symbolic.Operations (isZero, isNotZero)


-- Transformations:
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

isNotRedundant :: BVExpr -> Bool
isNotRedundant e = not $ any ($ e)
                   [ isNotNot
                   , isShr
                   , isPlusShr
                   , isTrivialIf
                   , isOp2Zero
                   , isLogicRepeat
                   , isLogicNotZero
                   , isWrongOrder
                   , isDeMorgan
                   , isConst
                   ]

isNotNot :: BVExpr -> Bool
isNotNot (Op1 Not (Op1 Not _)) = True
isNotNot _ = False

isShr :: BVExpr -> Bool
isShr (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 _)))) = True
isShr (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 _)))) = True
isShr _ = False

isPlusShr :: BVExpr -> Bool
isPlusShr (Op2 Plus e1 (Op2 Plus e2 _)) = e1 `like` e2
isPlusShr (Op2 Plus e1 e2)              = e1 `like` e2
isPlusShr _ = False

isTrivialIf :: BVExpr -> Bool
isTrivialIf (If0 Zero _ _) = True
isTrivialIf (If0 _ e1 e2)  = e1 `like` e2
isTrivialIf _ = False

isOp2Zero :: BVExpr -> Bool
isOp2Zero (Op2 _ _ Zero)  = True
isOp2Zero (Op2 _ Zero _)  = True
isOp2Zero _ = False

isLogicRepeat :: BVExpr -> Bool
isLogicRepeat (Op2 And e1 e2) = e1 `like` e2
isLogicRepeat (Op2 Or e1 e2)  = e1 `like` e2
isLogicRepeat (Op2 Xor e1 e2) = e1 `like` e2
isLogicRepeat (Op2 op1 e1 (Op2 op2 e2 _)) = op1 == op2 && e1 `like` e2
isLogicRepeat _ = False

isLogicNotZero :: BVExpr -> Bool
isLogicNotZero (Op2 _ _ (Op1 Not Zero)) = True
isLogicNotZero (Op2 _ (Op1 Not Zero) _) = True
isLogicNotZero _ = False

isWrongOrder :: BVExpr -> Bool
isWrongOrder (Op2 op1 e1 (Op2 op2 e2 _)) = op1 == op2 && e1 > e2
isWrongOrder (Op2 _ e1 e2)               = e1 > e2
isWrongOrder _ = False

isDeMorgan :: BVExpr -> Bool
isDeMorgan (Op2 Or (Op1 Not _) (Op1 Not _))  = True
isDeMorgan (Op2 And (Op1 Not _) (Op1 Not _)) = True
isDeMorgan _ = False

isConst :: BVExpr -> Bool
isConst e = isClosed e &&
            any (\c -> e /= c && evalExpr [] e == evalExpr [] c)
            [Zero, One, Op1 Not Zero, Op1 Not One]
