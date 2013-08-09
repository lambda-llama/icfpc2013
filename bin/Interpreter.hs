module Main where

import Prelude hiding (getLine)

import Control.Applicative ((<$>))
import Control.Monad (forever)

import Data.ByteString.Char8 (getLine)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as S

import Language.BV.Eval (evalProgram)
import Language.BV.Parser (bvProgramP)


main :: IO ()
main = forever $ do
    l     <- getLine
    input <- read . S.unpack <$> getLine
    case parseOnly bvProgramP l of
        Left err      -> putStrLn err
        Right program -> print $ evalProgram program input
