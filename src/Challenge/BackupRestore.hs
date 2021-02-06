{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Challenge.BackupRestore
  ( main
  ) where

import qualified Codec.Compression.GZip     as GZip
import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as Text
import           Hackattic.Network
import           Protolude
import           Text.InterpolatedString.QM
import           Text.Regex.PCRE

newtype Input
  = Input { dump :: Text }
  deriving (Generic, Show)
instance FromJSON Input

newtype Output
  = Output { alive_ssns :: [Text] }
  deriving (Generic, Show)
instance ToJSON Output

solve :: LBS.ByteString -> Output
solve bs =
  let needle = [qn|\d{3}-\d{2}-\d{4}|] :: [Char]
      lns    = bs & LBS.toStrict & decodeUtf8 & lines & filter
        ("alive" `Text.isInfixOf`)
      search :: [Char] -> [[Char]]
      search ln = getAllTextMatches (ln =~ needle)
  in  Output $ concatMap (map toS . search . toS) lns

main :: IO ()
main = do
  fetch "backup_restore"
    >>= (dump >>> unsafebase64decode)
    >>= (GZip.decompress >>> solve >>> submit "backup_restore")
