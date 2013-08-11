{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (id)

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Text.Printf (printf)
import qualified Control.Exception as E

import Data.Aeson ((.=), (.:), decode', encode, object)
import Data.Aeson.Types (Object, Pair, parseMaybe)
import Network.HTTP.Conduit (HttpException(..), RequestBody(..),
                             requestBody, responseBody, method,
                             parseUrl, withManager, httpLbs)
import qualified Data.HashMap.Strict as HashMap

import Language.BV.Types (BVProgram)

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
    unsafeSendRequest uri o
    `E.catch`
    \(StatusCodeException status _hdrs _cookies) -> do
        print status
        threadDelay 2000
        sendRequest uri o

evalID :: String -> [Int] -> IO (Maybe [Word64])
evalID id input = do
    result <- sendRequest "eval"
              [ "id"        .= id
              , "arguments" .= (map (printf "0x%X") input :: [String])
              ]
    return $! map read <$> parseMaybe (.: "outputs") result

guess :: String -> BVProgram -> IO Object
guess id program =
    sendRequest "guess" ["id" .= id, "program" .= show program]


data TrainRequest  = Size Int | Operators [String]

train :: TrainRequest -> IO Object
train (Size size)     = sendRequest "train" ["size" .= size]
train (Operators ops) = sendRequest "train" ["operators" .= ops]


main :: IO ()
main = do
    result <- train $ Size 30
    case parseMaybe (.: "id") result of
        Just id -> evalID id [42] >>= print
        Nothing -> error ":("
