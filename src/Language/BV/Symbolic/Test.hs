module Language.BV.Symbolic.Test where

import System.Random (getStdGen, randoms)
import Control.Monad(forM_)


import Language.BV.Gen
import Language.BV.Eval
import Language.BV.Types
import Language.BV.Symbolic.SEval
import Language.BV.Symbolic.Types

main :: IO ()
main = do
    r <- getStdGen
    let ops = operators
    let size = 8
    let exprs = genExpr ops size
    forM_ exprs $ \e ->
        let x = head $ randoms r
            ctx = [('x', x)]
            ret = word2sword $ evalExpr ctx e

            sret = sevalExpr stdContext e
        in if good sret ret (word2sword x)
           then print("OK")
           else do print("!!!")
                   print(e)
                   print(x)
                   print(ret)
                   print(sret)
                   error("LOOBSTER!")

good :: Sword -> Sword -> Sword -> Bool
good s e x = if length s /= 64 || length e /= 64
              then error "Length is not 64!!!!!!!!!!"
              else all eq (zip s e)
  where
    eq (i, j) = case (i, j) of
        (Sone, Sone) -> True
        (Sone, _) -> False
        (Szero, Szero) -> True
        (Szero, _) -> False
        (Bot, _) -> True
        (B ii, jj) -> if ii > 0
                    then x !! (fromIntegral $ ii-1) == jj
                    else x !! (fromIntegral $ (-ii)-1) == complementSbit jj
