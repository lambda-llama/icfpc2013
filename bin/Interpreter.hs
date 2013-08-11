module Main where

import Prelude hiding (getLine)

import Control.Applicative ((<$>))
import System.IO (isEOF)

import Data.ByteString.Char8 (getLine)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S

import Language.BV.Eval (evalProgram)
import Language.BV.Parser (bvProgramP)
import Language.BV.Util (whileM_)

main :: IO ()
main = whileM_ (not <$> isEOF) $ do
    l     <- getLine
    input <- read . S.unpack <$> getLine
    case parseOnly bvProgramP l of
        Left err      -> putStrLn err
        Right program -> print $ evalProgram program input
