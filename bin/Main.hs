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
meaningfulInputs = 42 : map bit [1..63]

main :: IO ()
main = do
    size <- read <$> getLine
    ops  <- read <$> getLine
    r    <- getStdGen
    let inputs = meaningfulInputs ++
                 (take (256 - length meaningfulInputs) $ randoms r)
        eqCls  = HashMap.toList $
                 HashMap.fromListWith (++) $
                 [ ([evalExpr expr [("x", x)] | x <- inputs], [expr])
                 | expr <- genExpr ops (pred size)
                 ]
        eqIds  = HashMap.fromList $ zip [1..] eqCls

    printHex inputs
    print $ HashMap.size eqIds
    forM_ (HashMap.toList eqIds) $ \(eqId :: Int, (res, exprs)) -> do
        print $ eqId          -- eq. class ID
        print $ length exprs  -- nr. of elements in eq. class
        printHex res          -- program output
        forM_ exprs $ \expr -> print $ BVProgram ("x", expr)

    go eqIds
  where
    printHex xs =
        putStrLn . intercalate " " $ ["0x" ++ showHex x "" | x <- xs]

    go eqIds = do
      eqId   <- read <$> getLine
      input  <- read <$> getLine
      output <- read <$> getLine
      let (res, exprs) = eqIds HashMap.! eqId
          correct      =
              filter (\expr -> evalExpr expr [("x", input)] == output) exprs

      print . length $ correct
      forM_ correct $ \expr ->
          print $ BVProgram ("x", expr)
      go (HashMap.insert eqId (res, correct) eqIds)
