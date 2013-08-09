module Main where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as S

import Data.Attoparsec.ByteString.Char8 (parseTest)

import Language.BV.Parser (bvProgramP)


main :: IO ()
main = forever $ parseTest bvProgramP =<< S.getLine
