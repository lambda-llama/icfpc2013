{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.BV.Parser
  ( bvProgramP
  , bvExprP
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.ByteString.Char8 as S

import Data.Attoparsec.ByteString.Char8 (Parser, choice, satisfy,
                                         peekChar, char, string, skipSpace)

import Language.BV.Types


bvIdP :: Parser BVId
bvIdP = do
    ch   <- satisfy isAlpha
    next <- peekChar
    case next of
        Just other | isAlphaNum other -> error "bvIdP: id too long"
        _ -> return ch

bvFoldP :: Parser BVFold
bvFoldP = between '(' ')' $ do
    token "fold"
    bvfArg  <- spaced bvExprP
    bvfInit <- spaced bvExprP
    between '(' ')' $ do
        token "lambda"
        (larg0, larg1) <- between '(' ')' $
                          (,) <$> spaced bvIdP <*> spaced bvIdP
        le <- spaced bvExprP
        return $ BVFold { bvfLambda = (larg0, larg1, le), .. }

bvOp1P :: Parser BVOp1
bvOp1P = choice
         [ "not"   *> pure Not
         , "shl1"  *> pure Shl1
         , "shr16" *> pure Shr16
         , "shr1"  *> pure Shr1
         , "shr4"  *> pure Shr4
         ]

bvOp2P :: Parser BVOp2
bvOp2P = choice
         [ token "and"  *> pure And
         , token "or"   *> pure Or
         , token "xor"  *> pure Xor
         , token "plus" *> pure Plus
         ]

bvExprP :: Parser BVExpr
bvExprP = choice
          [ if0
          , Fold <$> bvFoldP
          , op2
          , op1
          , Id <$> bvIdP
          , zeroOne
          ]
  where
    if0 = between '(' ')' $ do
        token "if0"
        If0 <$> spaced bvExprP
            <*> spaced bvExprP
            <*> spaced bvExprP

    op1 = between '(' ')' $ Op1 <$> bvOp1P <*> spaced bvExprP
    op2 = between '(' ')' $ Op2 <$> bvOp2P <*> spaced bvExprP <*> spaced bvExprP

    zeroOne = spaced $ choice
              [ char '0' *> pure Zero
              , char '1' *> pure One
              ]

bvProgramP :: Parser BVProgram
bvProgramP = between '(' ')' $ do
    token "lambda"
    arg <- between '(' ')' $ spaced bvIdP
    e   <- spaced bvExprP
    return $ BVProgram (arg, e)


spaced :: Parser a -> Parser a
spaced p = skipSpace *> p <* skipSpace
{-# INLINE spaced #-}

token :: S.ByteString -> Parser ()
token = void . spaced . string
{-# INLINE token #-}

between :: Char -> Char -> Parser a -> Parser a
between open close p = char open *> p <* char close
{-# INLINE between #-}
