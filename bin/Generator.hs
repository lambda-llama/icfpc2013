{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Bits (bit)
import Data.List (intercalate)
import Data.Word (Word64)
import Numeric (showHex)
import System.Random (mkStdGen, randoms)

import Control.Parallel.Strategies (using, parBuffer, rseq)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as VU

import Language.BV.Eval (evalExpr)
import Language.BV.Gen (genExpr)

instance Hashable (VU.Vector Word64) where
    hashWithSalt = VU.foldl' hashWithSalt

meaningfulInputs :: [Word64]
meaningfulInputs = 42 : map bit [0..64]

main :: IO ()
main = do
    size <- read <$> getLine
    ops  <- read <$> getLine
    r    <- return $ mkStdGen 42
    let exprs = genExpr ops (pred size)
        !inputs = meaningfulInputs ++
                  (take (256 - length meaningfulInputs) $ randoms r)
        !eqCls  = HashMap.fromListWith (\[expr] rest -> expr : rest) $!
                  ([ (VU.fromListN 256 $ map (\x -> evalExpr [('x', x)] expr) inputs,
                      [expr])
                   | expr <- exprs
                   ] `using` parBuffer (bit 32) rseq)
    
    forM_ (HashMap.keys eqCls) (\res ->
                                 let exprs' = eqCls HashMap.! res in do
                                     forM_ exprs' print)
    print $ HashMap.size eqCls
    print $ length exprs
