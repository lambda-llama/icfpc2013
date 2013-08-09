module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Bits (bit)
import Data.List (intercalate)
import Data.Word (Word64)
import Numeric (showHex)
import System.Random (getStdGen, randoms)

import Language.BV.Eval (evalExpr)
import Language.BV.Gen (genExpr)
import Language.BV.Types (BVProgram(..))


meaningfulInputs :: [Word64]
meaningfulInputs = map bit [1..63]

main :: IO ()
main = do
    size <- read <$> getLine
    ops  <- read <$> getLine
    r    <- getStdGen
    let exprs  = genExpr ops (pred size)
        inputs = meaningfulInputs ++
                 (take (256 - length meaningfulInputs) $ randoms r)

    print $ length exprs
    printHex inputs
    forM_ exprs $ \expr -> do
        print $ BVProgram ("x", expr)
        printHex [evalExpr expr [("x", x)] | x <- inputs]
  where
    printHex xs =
        putStrLn . intercalate " " $ ["0x" ++ showHex x "" | x <- xs]
