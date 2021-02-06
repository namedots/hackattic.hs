{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Challenge.HelpMeUnpack
  ( runChallenge
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Binary
import qualified Data.Binary.Get     as Get
import           Hackattic.Network
import           Protolude

newtype Input
  = Input { bytes :: Text }
  deriving (Generic, Show)
instance FromJSON Input

data Output
  = Output
      { int               :: Int32
      , uint              :: Word32
      , short             :: Int16
      , float             :: Double
      , double            :: Double
      , big_endian_double :: Double
      }
  deriving (Generic, Show)
instance ToJSON Output
instance Binary Output where
  get =
    Output
      <$> Get.getInt32le
      <*> Get.getWord32le
      <*> (Get.getInt16le <* Get.getWord16le) -- ignore 16 bits after
      <*> (realToFrac <$> Get.getFloatle) -- is float but str repr of double
      <*> Get.getDoublele
      <*> Get.getDoublebe

runChallenge :: IO ()
runChallenge = do
  bs <- unsafebase64decode . bytes =<< fetch "help_me_unpack"
  let output :: Output = Data.Binary.decode bs
  print $ Data.Aeson.encode output
  submit "help_me_unpack" output
