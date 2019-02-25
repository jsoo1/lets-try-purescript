{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Concurrent.MVarLock   (Lock, newLock)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TVar   (TVar, modifyTVar, newTVar,
                                                readTVar)
import Control.Exception (catch, throw)
import           Control.Monad.IO.Class        (liftIO)
import           Data                          (Dir (..), Message (..),
                                                TimeCreated (..), User,
                                                Username, by, username)
import qualified Data.Aeson                    as AE
import qualified Data.Aeson.Text               as AE
import Data.Functor (void)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Proxy                    (Proxy (..))
import Data.Text (Text)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import Data.UUID (UUID)
import           DB                            (DB (..), KV)
import qualified DB
import           Network.Wai.Handler.Warp      (run)
import           Network.WebSockets            (WebSocketsData, DataMessage, ConnectionException(..))
import           Network.WebSockets.Connection (Connection, forkPingThread,
                                                receiveDataMessage, sendClose, sendTextData)
import           Options.Generic               (Generic, ParseRecord, getRecord)
import           Servant
import           Servant.API.WebSocket
import System.Random (randomIO)

data Options =
  Options
  { from     :: FilePath
  , on       :: Int
  , users    :: FilePath
  , messages :: FilePath
  }
  deriving (Generic, ParseRecord)

type LetsTryPureScript = "user" :> ReqBody '[JSON] Username :> Get '[JSON] (Maybe User)
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> "user" :> ReqBody '[JSON] Username :> DeleteNoContent '[JSON] NoContent
  :<|> "user" :> "all" :> Get '[JSON] (Map Username User)
  :<|> "user" :> "subscribe" :> WebSocket
  :<|> "message" :> ReqBody '[JSON] (Username, TimeCreated) :> Get '[JSON] (Maybe Message)
  :<|> "message" :> ReqBody '[JSON] Message :> Post '[JSON] Message
  :<|> "message" :> ReqBody '[JSON] (Username, TimeCreated) :> Get '[JSON] NoContent
  :<|> "message" :> "all" :> Get '[JSON] (Map (Username, TimeCreated) Message)
  :<|> "message" :> "subscribe" :> WebSocket
  :<|> Raw

data State =
  State
  { userConnections    :: TVar (Map UUID Connection)
  , messageConnections :: TVar (Map UUID Connection)
  }

main :: IO ()
main = do
  Options {..} <- getRecord "serve a static directory and a users \"database\""
  putStrLn $ "running on " <> show on
  lockUsers <- newLock
  lockMessages <- atomically $ newTVar Map.empty
  userConnections <- atomically $ newTVar Map.empty
  messageConnections <- atomically $ newTVar Map.empty
  run on
    $ serve letsTryPureScript
    $ server
    (State userConnections messageConnections)
    (Dir from)
    (Users users lockUsers)
    (Messages (Dir messages) lockMessages)

  where
    letsTryPureScript :: Proxy LetsTryPureScript
    letsTryPureScript = Proxy

server :: State -> Dir -> DB Username User -> DB (Username, TimeCreated) Message -> Server LetsTryPureScript
server state (Dir staticDir) users messages =
  get users
  :<|> (\user -> do
          u <- post users (username user) user
          broadcast (userConnections state) (AE.encodeToLazyText u)
          pure u)
  :<|> delete users
  :<|> getAll users
  :<|> subscribe (userConnections state)
  :<|> get messages
  :<|> (\msg -> do
           now <- TimeCreated <$> liftIO getPOSIXTime
           m <- post messages (by msg, now) $ Message (by msg) now (content msg)
           broadcast (messageConnections state) (AE.encodeToLazyText m)
           pure m)
  :<|> delete messages
  :<|> getAll messages
  :<|> subscribe (messageConnections state)
  :<|> serveDirectoryWebApp staticDir

  where
    getAll :: KV k v => DB k v -> Handler (Map k v)
    getAll db = liftIO (DB.all db) >>= orErr

    get :: KV k v => DB k v -> k -> Handler (Maybe v)
    get db k = liftIO (DB.get db k) >>= orErr

    post :: KV k v => DB k v -> k -> v -> Handler v
    post db k v = liftIO (DB.insert db k v) >>= orErr

    delete :: KV k v => DB k v -> k -> Handler NoContent
    delete db k = do
      liftIO (DB.delete db k) >>= orErr
      pure NoContent

    subscribe :: TVar (Map UUID Connection) -> Connection -> Handler ()
    subscribe cs c = do
      liftIO $ forkPingThread c 30
      uuid <- liftIO $ randomIO
      liftIO $ atomically $ modifyTVar cs (Map.insert uuid c)
      liftIO $ catch (void $ receiveDataMessage c) ((\case
        ConnectionClosed -> do
          atomically $ modifyTVar cs (Map.delete uuid)
          sendClose c ("closing now" :: Text)
        CloseRequest _ _ -> atomically $ modifyTVar cs (Map.delete uuid)
        e -> throw e) :: ConnectionException -> IO ())

    broadcast :: WebSocketsData a => TVar (Map UUID Connection) -> a -> Handler ()
    broadcast cs x = do
      conns <- liftIO $ atomically $ readTVar cs
      liftIO $ flip sendTextData x `traverse` conns
      pure ()

    orErr :: Either String a -> Handler a
    orErr = either (throwError . serverError) pure

    serverError :: String -> ServantErr
    serverError s =
      ServantErr
      { errHTTPCode = 500
      , errReasonPhrase = s
      , errBody = ""
      , errHeaders = []
      }
