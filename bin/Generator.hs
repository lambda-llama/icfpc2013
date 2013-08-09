{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Bits (bit)
import Data.List (intercalate)
import Data.Word (Word64)
import Numeric (showHex)
import System.Random (getStdGen, randoms)
import qualified Data.HashMap.Strict as HashMap

import Language.BV.Eval (evalExpr)
import Language.BV.Gen (genExpr)
import Language.BV.Types (BVProgram(..))


meaningfulInputs :: [Word64]
meaningfulInputs = 42 : map bit [0..64]

main :: IO ()
main = do
    size <- read <$> getLine
    ops  <- read <$> getLine
    r    <- getStdGen
    let inputs = meaningfulInputs ++
                 (take (512 - length meaningfulInputs) $ randoms r)
        eqCls  = HashMap.fromListWith (++) $
                 [ ([evalExpr expr [("x", x)] | x <- inputs], [expr])
                 | expr <- genExpr ops (pred size)
                 ]

    printHex inputs
    print $ HashMap.size eqCls
    forM_ (zip [1..] $ HashMap.toList eqCls) $ \(eqId, (res, exprs)) -> do
        print $ (eqId :: Int) -- eq. class ID
        print $ length exprs  -- nr. of elements in eq. class
        printHex res          -- program output
        forM_ exprs $ \expr -> print $ BVProgram ("x", expr)
  where
    printHex xs =
        putStrLn . intercalate " " $ ["0x" ++ showHex x "" | x <- xs]
