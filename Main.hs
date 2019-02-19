{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (FromJSON, FromJSONKey, ToJSON,
                                           ToJSONKey)
import qualified Data.Aeson               as AE
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Proxy               (Proxy (..))
import           Data.Text
import           Network.Wai.Handler.Warp (run)
import           Options.Generic          (Generic, ParseRecord, getRecord)
import           Servant

data Options =
  Options
  { from  :: FilePath
  , on    :: Int
  , users :: FilePath
  }
  deriving (Generic, ParseRecord)

type LetsTryPureScript = "users" :> Get '[JSON] (Map Username User)
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> Raw

main :: IO ()
main = do
  Options {..} <- getRecord "serve a static directory and a users \"database\""
  putStrLn $ "running on " <> show on
  run on $ serve letsTryPureScript $ server from users

data User =
  User
  { username  :: Username
  , name      :: Name
  , avatarUrl :: ImgSrc
  , url       :: UserURL
  , bio       :: Bio
  }
  deriving (Generic, FromJSON, ToJSON)

letsTryPureScript :: Proxy LetsTryPureScript
letsTryPureScript = Proxy

server :: FilePath -> FilePath -> Server LetsTryPureScript
server staticDir usersFile =
  getUsers usersFile :<|> postUser usersFile :<|> serveDirectoryWebApp staticDir

serverError :: String -> ServantErr
serverError s =
  ServantErr
  { errHTTPCode = 500
  , errReasonPhrase = s
  , errBody = ""
  , errHeaders = []
  }

getUsersFile :: FilePath -> IO (Either String (Map Username User))
getUsersFile p =
  AE.eitherDecodeFileStrict p :: IO (Either String (Map Username User))

getUsers :: FilePath -> Handler (Map Username User)
getUsers p =
  either (throwError . serverError) pure =<< liftIO (getUsersFile p)

postUser :: FilePath -> User -> Handler User
postUser p user =
  either (throwError . serverError) (updateUser user) =<< liftIO (getUsersFile p)
    where
      updateUser :: User -> Map Username User -> Handler User
      updateUser u us = do
        liftIO $ AE.encodeFile p $ Map.insert (username u) u us
        pure u

-------------------- Plumbing --------------------

newtype Username = Username Text
  deriving (Eq, Ord, Show, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

newtype ImgSrc = ImgSrc Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype UserURL = UserURL Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype Name = Name Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype Bio = Bio Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
