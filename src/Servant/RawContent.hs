{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Module      :  Servant.RawContent
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- An 'RawContent' empty data type with `MimeRender` instances for 'ByteString'.  You
-- should only need to import this module for it's instances and the `RawContent`
-- datatype.:
--
-- >>> type SomeGet = Get '[RawContent "application/ex"] ByteString
--
-- This is extension of 'OctetStream' type:
--
-- >>> type OctetStream = RawContent "application/octet-stream"
module Servant.RawContent (RawContent) where

import Data.Maybe   (fromMaybe)
import Data.Proxy   (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API  (Accept (..), MimeRender (..), MimeUnrender (..))

import           Prelude
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Media   as M

data RawContent (s :: Symbol)

instance KnownSymbol s => Accept (RawContent s) where
    contentType _ = fromMaybe "application/octet-stream"
                  . M.parseAccept
                  . T.encodeUtf8
                  . T.pack
                  $ symbolVal (Proxy :: Proxy s)

instance KnownSymbol s => MimeRender (RawContent s) BS.ByteString where
    mimeRender _ = LBS.fromStrict

instance KnownSymbol s  => MimeRender (RawContent s) LBS.ByteString where
    mimeRender _ = id

instance KnownSymbol s => MimeUnrender (RawContent s) BS.ByteString where
    mimeUnrender _ = return . LBS.toStrict

instance KnownSymbol s => MimeUnrender (RawContent s) LBS.ByteString where
    mimeUnrender _ = return
