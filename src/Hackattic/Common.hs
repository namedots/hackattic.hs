{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Hackattic.Common
  ( submit
  , fetch
  , unsafebase64decode
  , getUnixTime
  , getUnixTimeMs
  ) where

import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Hackattic.Config       as Config
import           Network.HTTP.Req
import           Protolude
import qualified Protolude.Conv         as PartialConv

token :: Text
token = Config.initialConfig & Config.accessToken

fetch :: FromJSON a => Text -> IO a
fetch problem = runReq defaultHttpConfig $ do
  let url = https "hackattic.com" /: "challenges" /: problem /: "problem"
  responseBody <$> req GET url NoReqBody jsonResponse ("access_token" =: token)

submit :: (ToJSON a, Show a) => Text -> a -> IO ()
submit problem jsonable = runReq defaultHttpConfig $ do
  let url = https "hackattic.com" /: "challenges" /: problem /: "solve"
  putText $ "submitting: " <> show jsonable
  res <-
    req POST url (ReqBodyJson jsonable) bsResponse
    $  ("access_token" =: token)
    <> ("playground" =: (1 :: Int))
  liftIO $ putStrLn (responseBody res)

unsafebase64decode
  :: ( PartialConv.StringConv s1 ByteString
     , PartialConv.StringConv ByteString s2
     )
  => s1
  -> IO s2
unsafebase64decode s = case B64.decode (PartialConv.toS s) of
  Right bs  -> pure (PartialConv.toS bs)
  Left  err -> print err >> exitFailure

getUnixTime :: IO Int
getUnixTime = (`div` 1000) <$> getUnixTimeMs

getUnixTimeMs :: IO Int
getUnixTimeMs = ceiling . (* 1000) . utcTimeToPOSIXSeconds <$> getCurrentTime
