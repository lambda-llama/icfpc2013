module Language.BV.Simplifier
  ( simplify
  ) where

import Language.BV.Types

-- Transformations:
--
-- not (not e)             = e
-- (or e 0)                = (or 0 e)  = e
-- (and e 1)               = (and 1 e) = e
-- (xor e e)               = 0
-- (plus e 0) = (plus 0 e) = e
-- (shr4 (shr4 (shr4 (shr4 e)))) = (shr16 e)
-- (shr1 (shr1 (shr1 (shr1 e)))) = (shr4 e)

simplify :: BVExpr -> Either BVExpr BVExpr
simplify (Op1 Not (Op1 Not e))   = Right e
simplify (Op2 Or e Zero)         = Right e
simplify (Op2 Or Zero e)         = Right e
simplify (Op2 And e One)         = Right e
simplify (Op2 And One e)         = Right e
simplify (Op2 Plus e Zero)       = Right e
simplify (Op2 Plus Zero e)       = Right e
simplify (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 (Op1 Shr4 e)))) = Right (Op1 Shr16 e)
simplify (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 (Op1 Shr1 e)))) = Right (Op1 Shr4 e)
simplify e = mix e

mix :: BVExpr -> Either BVExpr BVExpr
mix (If0 Zero e1 _e2) = Right e1
mix (If0 One _e1 e2)  = Right e2
mix (Op1 Not One)     = Right Zero
mix (Op1 Not Zero)    = Right One
mix (Op2 Plus Zero One)  = Right One
mix (Op2 Plus Zero Zero) = Right Zero
mix e = Left e  -- Note(superbobry): add boolean expression!
