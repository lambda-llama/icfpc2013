{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, zipWithM_)
import Data.Bits (bit)
import Data.List (intercalate)
import Data.Word (Word64)
import Numeric (showHex)
import System.Random (getStdGen, randoms)

import Control.Parallel.Strategies (using, parBuffer, rseq)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as VU

import Language.BV.Eval (evalExpr)
import Language.BV.Gen (genExpr)
import Language.BV.Types (BVProgram(..))

instance Hashable (VU.Vector Word64) where
    hashWithSalt = VU.foldl' hashWithSalt

meaningfulInputs :: [Word64]
meaningfulInputs = 42 : map bit [0..64]

main :: IO ()
main = do
    size <- read <$> getLine
    ops  <- read <$> getLine
    r    <- getStdGen
    let !inputs = meaningfulInputs ++
                  (take (256 - length meaningfulInputs) $ randoms r)
        !eqCls  = HashMap.fromListWith (\[expr] rest -> expr : rest) $!
                  ([ (VU.fromListN 256 [evalExpr expr [('x', x)] | x <- inputs],
                      [expr])
                   | expr <- genExpr ops (pred size)
                   ] `using` parBuffer (bit 32) rseq)

    printHex inputs
    print $ HashMap.size eqCls
    zipWithM_ (\eqId res ->
                let exprs = eqCls HashMap.! res in do
                    print $ (eqId :: Int)     -- eq. class ID
                    print $ length exprs      -- nr. of elements in eq. class
                    printHex $ VU.toList res  -- program output
                    forM_ exprs $ \expr -> print $ BVProgram ('x', expr))
        [1..] (HashMap.keys eqCls)
  where
    printHex xs =
        putStrLn . intercalate " " $ ["0x" ++ showHex x "" | x <- xs]
