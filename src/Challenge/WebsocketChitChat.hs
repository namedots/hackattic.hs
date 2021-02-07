{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Challenge.WebsocketChitChat
  ( main
  ) where

import           Control.Arrow      hiding (loop)
import           Control.Monad
import           Data.Aeson
import           Data.IORef
import           Data.List.Extra    (minimumOn)
import qualified Data.Text          as Text
import           Hackattic.Common   (fetch, getUnixTimeMs, submit)
import           Network.WebSockets (ClientApp, receiveData, sendClose,
                                     sendTextData)
import           Protolude
import           Wuss

newtype Problem
  = Problem { token :: Text }
  deriving (Generic, Show)
instance FromJSON Problem

newtype Submission
  = Submission { secret :: Text }
  deriving (Generic, Show)
instance ToJSON Submission

main :: IO ()
main = do
  t <- token <$> fetch "websocket_chit_chat"
  runSecureClient "hackattic.com" 443 (toS $ "/_/ws/" <> t) ws

ws :: ClientApp ()
ws connection = do
  let recv = do
        message <- receiveData connection
        putText $ "< " <> show message
        pure message
      send (message :: Text) = do
        sendTextData connection message
        putText $ "> " <> show message
  void recv
  prevR <- getUnixTimeMs >>= newIORef
  let
    loop = do
      message <- recv
      when (message == "ping!") $ do
        now  <- getUnixTimeMs
        prev <- readIORef prevR
        writeIORef prevR now
        let elapsed  = now - prev
            response = minimumOn (subtract elapsed >>> abs)
                                 [700, 1500, 2000, 2500, 3000]
        send (show response)
      if "congratulations" `Text.isPrefixOf` message
        then do
          message
            & Text.dropWhile (/= '\"')
            & Text.drop 1
            & Text.dropEnd 1
            & Submission
            & submit "websocket_chit_chat"
          sendClose connection ("byeeeeeeee I love you" :: Text)
        else loop
  loop
