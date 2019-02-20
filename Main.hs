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

type LetsTryPureScript = "user" :> ReqBody '[JSON] Username :> Get '[JSON] (Maybe User)
  :<|> "user" :> Get '[JSON] (Map Username User)
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> "user" :> ReqBody '[JSON] Username :> Delete '[JSON] ()
  :<|> Raw

main :: IO ()
main = do
  Options {..} <- getRecord "serve a static directory and a users \"database\""
  putStrLn $ "running on " <> show on
  run on $ serve letsTryPureScript $ server from users

data User =
  User
  { username  :: Username
  , name      :: Text
  , avatarUrl :: Text
  , url       :: Text
  , bio       :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

letsTryPureScript :: Proxy LetsTryPureScript
letsTryPureScript = Proxy

server :: FilePath -> FilePath -> Server LetsTryPureScript
server staticDir usersFile =
  getUser usersFile
  :<|> getUsersFile usersFile
  :<|> postUser usersFile
  :<|> deleteUser usersFile
  :<|> serveDirectoryWebApp staticDir

  where
    getUsersFile :: FilePath -> Handler (Map Username User)
    getUsersFile p =
      either (throwError . serverError) pure =<< liftIO (AE.eitherDecodeFileStrict p :: IO (Either String (Map Username User)))

    getUser :: FilePath -> Username -> Handler (Maybe User)
    getUser p u =
      Map.lookup u <$> getUsersFile p

    postUser :: FilePath -> User -> Handler User
    postUser p user =
      updateUser p user =<< getUsersFile p
      where
        updateUser :: FilePath -> User -> Map Username User -> Handler User
        updateUser p' u us = do
          liftIO $ AE.encodeFile p' $ Map.insert (username u) u us
          pure u

    deleteUser :: FilePath -> Username -> Handler ()
    deleteUser p username =
      delete p username =<< getUsersFile p
      where
        delete :: FilePath -> Username -> Map Username User -> Handler ()
        delete p' u us = do
          liftIO $ AE.encodeFile p' $ Map.delete u us
          pure ()

    serverError :: String -> ServantErr
    serverError s =
      ServantErr
      { errHTTPCode = 500
      , errReasonPhrase = s
      , errBody = ""
      , errHeaders = []
      }

newtype Username = Username Text
  deriving (Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)
