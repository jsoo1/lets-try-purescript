{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Monad.IO.Class   (liftIO)
import           Data                     (Dir (..), Message, TimeCreated(..), User,
                                           Username, by, username)
import qualified Data.Aeson               as AE
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Proxy               (Proxy (..))
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           DB                       (DB (..), KV)
import qualified DB
import           Network.Wai.Handler.Warp (run)
import           Options.Generic          (Generic, ParseRecord, getRecord)
import           Servant

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
  :<|> "message" :> ReqBody '[JSON] (Username, TimeCreated) :> Get '[JSON] (Maybe Message)
  :<|> "message" :> ReqBody '[JSON] Message :> Post '[JSON] Message
  :<|> "message" :> ReqBody '[JSON] (Username, TimeCreated) :> Get '[JSON] NoContent
  :<|> "message" :> "all" :> Get '[JSON] (Map (Username, TimeCreated) Message)
  :<|> Raw

main :: IO ()
main = do
  Options {..} <- getRecord "serve a static directory and a users \"database\""
  putStrLn $ "running on " <> show on
  run on $ serve letsTryPureScript $ server (Dir from) (Users users) (Messages (Dir messages))

  where
    letsTryPureScript :: Proxy LetsTryPureScript
    letsTryPureScript = Proxy

server :: Dir -> DB Username User -> DB (Username, TimeCreated) Message -> Server LetsTryPureScript
server (Dir staticDir) users messages =
  get users
  :<|> (pure (post users) <*> username <*> id)
  :<|> delete users
  :<|> getAll users
  :<|> get messages
  :<|> (\msg -> do
           now <- liftIO getPOSIXTime
           post messages (by msg, TimeCreated now) msg)
  :<|> delete messages
  :<|> getAll messages
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
