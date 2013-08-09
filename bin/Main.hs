module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.List (intercalate)
import Numeric (showHex)
import System.Random (getStdGen, randoms)

import Language.BV.Eval (evalExpr)
import Language.BV.Gen (genExpr)
import Language.BV.Types (BVProgram(..))


main :: IO ()
main = do
    size <- read <$> getLine
    ops  <- read <$> getLine
    r    <- getStdGen
    let exprs  = genExpr ops (pred size)
        inputs = take 256 $ randoms r

    printHex inputs
    forM_ exprs $ \expr -> do
        print $ BVProgram ("x", expr)
        printHex [evalExpr expr [("x", x)] | x <- inputs]
  where
    printHex xs =
        putStrLn . intercalate " " $ ["0x" ++ showHex x "" | x <- xs]
