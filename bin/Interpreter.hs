module Main where

import Prelude hiding (getLine)

import Control.Applicative ((<$>))
import System.IO (isEOF)

import Data.ByteString.Char8 (getLine)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S

import Language.BV.Eval (evalProgram)
import Language.BV.Parser (bvProgramP)

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = do
    x <- p
    if x
    then f >> whileM_ p f
    else return ()
{-# SPECIALIZE whileM_ :: IO Bool -> IO a -> IO () #-}

main :: IO ()
main = whileM_ (not <$> isEOF) $ do
    l     <- getLine
    input <- read . S.unpack <$> getLine
    case parseOnly bvProgramP l of
        Left err      -> putStrLn err
        Right program -> print $ evalProgram program input
