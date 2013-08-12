{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (id)

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Bits (bit)
import Data.Maybe (fromMaybe, fromJust)
import Data.Word (Word64)
import System.Environment (getArgs)
import System.Random (getStdGen, randoms)
import Text.Printf (printf)
import qualified Control.Exception as E

import Control.Parallel.Strategies (using, parBuffer, rseq)
import Data.Aeson ((.=), (.:), decode', encode, object)
import Data.Aeson.Types (Object, Pair, parseMaybe)
import Data.Hashable (Hashable(..))
import Network.HTTP.Conduit (HttpException(..), RequestBody(..),
                             requestBody, responseBody, method,
                             parseUrl, withManager, httpLbs)
import Network.HTTP.Types (statusCode)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as VU

import Language.BV.Eval (evalExpr)
import Language.BV.Gen (genExpr)
import Language.BV.Types (BVExpr, BVProgram(..))

instance Hashable (VU.Vector Word64) where
    hashWithSalt = VU.foldl' hashWithSalt

secret :: String
secret = "0047qni4Jv0ErZcl53WtPweKjim6oLQ6dqDLc9SzvpsH1H"

unsafeSendRequest :: String -> [Pair] -> IO Object
unsafeSendRequest uri o = do
    request  <- parseUrl $
                printf "http://icfpc2013.cloudapp.net/%s?auth=%s" uri secret
    response <- withManager . httpLbs $
                request { requestBody = RequestBodyLBS . encode $ object o
                        , method      = "POST"
                        }
    return $ fromMaybe HashMap.empty . decode' . responseBody $ response

sendRequest :: String -> [Pair] -> IO Object
sendRequest uri o =
    unsafeSendRequest uri o `E.catch` \(e :: HttpException) -> do
        putStrLn $ case e of
            StatusCodeException status _hdrs _cookies ->
                if statusCode status `elem` [410, 412]
                then error $ show status
                else show status
            _other -> show e
        threadDelay 4000
        sendRequest uri o

evalID :: String -> [Word64] -> IO [Word64]
evalID id input = do
    result <- sendRequest "eval"
              [ "id"        .= id
              , "arguments" .= (map (printf "0x%X") input :: [String])
              ]
    return $! map read . fromJust $ parseMaybe (.: "outputs") result

data Status = Win
            | Mismatch !(Word64, Word64, Word64)
            | Error !String
    deriving (Show, Eq)

guess :: String -> BVProgram -> IO Status
guess id program = do
    result <- sendRequest "guess" ["id" .= id, "program" .= show program]
    return $! fromJust $ do
        status <- parseMaybe (.: "status") result
        case status :: String of
            "win"      -> return Win
            "mismatch" -> do
                values <- map read <$> parseMaybe (.: "values") result
                return $ case values of
                    [input, output, answer] -> Mismatch (input, output, answer)
                    _other -> error "The impossible happened!"
            "error"    -> Error <$> parseMaybe (.: "error") result
            _          -> error "The impossible happened!"


data TrainRequest  = Size Int | Operators [String]

train :: TrainRequest -> IO Object
train (Size size)     = sendRequest "train" ["size" .= size]
train (Operators ops) = sendRequest "train" ["operators" .= ops]


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["f", n] -> fake $ read n
        ["r"]    -> real
        _other   -> putStrLn "usage: bvs [f N|r]"
  where
    fake :: Int -> IO ()
    fake size = forever $ do
        response <- train (Size size)
        let id   = fromJust $ parseMaybe (.: "id") response
            ops  = fromJust $ parseMaybe (.: "operators") response
        solve id size ops

    real :: IO ()
    real = do
        id   <- getLine
        size <- read <$> getLine
        ops  <- read <$> getLine
        solve id size ops

solve :: String -> Int -> [String] -> IO ()
solve id size ops = do
    putStrLn $ printf "%s %s %s" id (show size) (show ops)
    r <- getStdGen
    let !inputs = meaningfulInputs ++
                  (take (128 - length meaningfulInputs) $ randoms r)
        !envs   = map (\x -> [('x', x)]) inputs
        !eqCls  = HashMap.fromListWith (\[expr] rest -> expr : rest) $!
                  ([ (VU.fromListN 128 $ map (\env -> evalExpr env expr) envs,
                      [expr])
                   | expr <- genExpr ops (pred size)
                   ] `using` parBuffer (bit 32) rseq)

    outputs <- evalID id inputs
    bruteForce $ case HashMap.lookup (VU.fromList outputs) eqCls of
        Just exprs -> exprs
        Nothing    ->
            error (printf "Failed to find eq. class for %s with outputs %s!"
                   id (show outputs))
  where
    meaningfulInputs :: [Word64]
    meaningfulInputs = 42 : map bit [0..64]

    bruteForce :: [BVExpr] -> IO ()
    bruteForce [] = do
        putStrLn "  /==g           _ "
        putStrLn " //      >>>/---{_ "
        putStrLn " `==::[[[[|:     _ "
        putStrLn "         >>>\\---{_"
        error "Exausted candidate list, LOBSTERS!"
    bruteForce (candidate:exprs) = do
        status <- guess id (BVProgram ('x', candidate))
        putStrLn $ printf "%s %s %s" id (show status) (show candidate)
        case status of
            Win -> return ()
            Mismatch (input, output, _answer) ->
                bruteForce $!
                filter (\expr -> evalExpr [('x', input)] expr == output) exprs
            Error reason -> error reason
