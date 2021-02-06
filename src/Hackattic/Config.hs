{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Hackattic.Config
  ( Config(..)
  , getConfig
  , initialConfig
  ) where

import           Dhall
import           GHC.IO.Unsafe (unsafePerformIO)
import           Protolude

data Config
  = Config
      { host        :: Text
      , accessToken :: Text
      }
  deriving (Generic, Show)
instance FromDhall Config

-- File: ./secrets.dhall
-- { host = "12.34.56.78:91011", accessToken = "ffffffffffffffff" }

getConfig :: IO Config
getConfig = do
  input auto "./secrets.dhall"

initialConfig :: Config
{-# NOINLINE initialConfig #-}
initialConfig = unsafePerformIO getConfig
