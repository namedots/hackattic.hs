{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Challenge.JottingJWTS
  ( main
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8                as LBS
import           Data.Default                              (def)
import           Data.IORef
import qualified Hackattic.Config                          as Config
import           Hackattic.Common
import qualified Jose.Jws                                  as Jws
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           Protolude                                 hiding (Handler, exp)
import           Servant
import           Servant.RawContent

type API
  = ReqBody '[RawContent "application/octet-stream"] ByteString :> Post '[JSON] Reply

-- | fields of interest that the POST'ed jwts may have
data JWTData
  = JWTData
      { append :: Maybe Text
      , exp    :: Maybe Int
      , nbf    :: Maybe Int
      }
  deriving (Eq, Generic, Show)
instance FromJSON JWTData

-- | what I reply to POST's
newtype Reply
  = Reply { solution :: Maybe Text }
  deriving (Eq, Generic, Show)
instance ToJSON Reply

-- | initial problem description
newtype Problem
  = Problem { jwt_secret :: Text }
  deriving (Eq, Generic, Show)
instance FromJSON Problem

-- | submitted in response to the problem
newtype Solution
  = Solution { app_url :: Text }
  deriving (Eq, Generic, Show)
instance ToJSON Solution

handler :: ByteString -> IORef [Text] -> (ByteString -> Handler Reply)
handler secret ioref jwt = join $ liftIO $ do
  t <- getUnixTime
  putText "--------------------------------------------"
  putText $ "t=" <> show t
  putText "this what the (decoded) request looks like"
  let decodedJwt = Jws.hmacDecode secret jwt
  print decodedJwt
  case decodedJwt of
    Left  _       -> pure (throwError err400)
    Right (_, bs) -> case decode (LBS.fromStrict bs) of
      Nothing      -> pure (throwError err400)
      Just jwtdata -> do
        let expired = case exp jwtdata of
              Nothing     -> False
              Just expire -> expire < t
            tooEarly = case nbf jwtdata of
              Nothing        -> False
              Just notbefore -> notbefore > t
        putText $ "expired? " <> show expired
        putText $ "too early? " <> show tooEarly
        if not (expired || tooEarly)
          then case append jwtdata of
            Nothing -> do
              result <- readIORef ioref
              pure (pure (Reply (Just (mconcat (reverse result)))))
            Just appendMe -> do
              modifyIORef ioref (appendMe :)
              pure $ pure (Reply Nothing)
          else pure (throwError err400)

app :: ByteString -> IORef [Text] -> Application
app secret ioref = serve (Proxy :: Proxy API) (handler secret ioref)

main :: IO ()
main = do
  -- fetch the jwt secret / start the challenge
  prob <- fetch "jotting_jwts"
  print prob
  -- I'll be keeping the appendend strings here
  ioref     <- newIORef []
  -- please wait a split second and then tell hackattic I'm ready to receive
  -- requests. I'm forking that instead of the server, because this terminates
  -- on its own, which the server does not - the server will keep running in
  -- ghci when reloaded when forked, so better to keep it as the main thread.
  host      <- Config.getConfig <&> Config.host
  _threadId <- forkIO $ do
    threadDelay 100000
    putText "submitting app url"
    submit "jotting_jwts" (Solution ("http://" <> host))
  putText "starting server.."
  let appl = app (toUtf8 (jwt_secret prob)) ioref
  if False
    then do
      -- log request details as json
      logmw <- mkRequestLogger $ def
        { outputFormat = CustomOutputFormatWithDetailsAndHeaders
                           formatAsJSONWithHeaders
        }
      run 8082 $ logmw appl
    else do
      -- smaller logging output
      withStdoutLogger $ \aplogger -> do
        let settings = setPort 8082 $ setLogger aplogger defaultSettings
        runSettings settings appl
