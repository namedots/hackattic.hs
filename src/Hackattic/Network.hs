{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Hackattic.Network
  ( submit
  , fetch
  , unsafebase64decode
  ) where

import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import           Network.HTTP.Req
import qualified Hackattic.Config as Config
import           Protolude
import qualified Protolude.Conv         as PartialConv

token :: Text
token = Config.initialConfig & Config.accessToken

fetch :: FromJSON a => Text -> IO a
fetch problem = runReq defaultHttpConfig $ do
  let url = https "hackattic.com" /: "challenges" /: problem /: "problem"
  responseBody <$> req GET url NoReqBody jsonResponse ("access_token" =: token)

submit :: ToJSON a => Text -> a -> IO ()
submit problem jsonable = runReq defaultHttpConfig $ do
  let url = https "hackattic.com" /: "challenges" /: problem /: "solve"
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
