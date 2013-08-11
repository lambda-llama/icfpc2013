module Language.BV.Symbolic.Test where

import Control.Monad (forM_)
import Data.Bits (testBit)
import Data.Word (Word64)
import System.Random (getStdGen, randoms)

import qualified Data.Vector as V

import Language.BV.Gen
import Language.BV.Eval
import Language.BV.Types
import Language.BV.Symbolic.Operations
import Language.BV.Symbolic.SEval
import Language.BV.Symbolic.Types

main :: IO ()
main = do
    r <- getStdGen
    let ops   = operators
    let size  = 7
    let exprs = genExpr ops size
    forM_ exprs $ \e ->
        let x    = head $ randoms r
            ctx  = [('x', x)]
            ret  = word2sword $ evalExpr ctx e
            sret = sevalExpr stdContext e
        in if good sret ret (word2sword x)
           then print "OK"
           else do
               print e
               print x
               print (ret, V.length ret)
               print (sret, V.length sret)
               error "LOOBSTER!"

good :: Sword -> Sword -> Sword -> Bool
good s e x = if V.length s /= 64 || V.length e /= 64
              then False
              else V.all eq (V.zip s e)
  where
    eq (i, j) = case (i, j) of
        (Sone, Sone) -> True
        (Sone, _) -> False
        (Szero, Szero) -> True
        (Szero, _) -> False
        (Bot, _) -> True
        (B ii, jj) -> if ii > 0
                    then x V.! (fromIntegral $ ii-1) == jj
                    else x V.! (fromIntegral $ (-ii)-1) == complementSbit jj

word2sword :: Word64 -> Sword
word2sword w = V.fromList
               [if w `testBit` i then Sone else Szero | i <- [63,62..0]]
