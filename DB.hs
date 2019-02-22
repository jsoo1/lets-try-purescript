{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}

module DB (DB(..), all, get, insert) where

import Data (Dir, Message, Username(..), TimeCreated, User, by, created)
import Data.Kind (Type)
import Data.Aeson (FromJSONKey, FromJSON, ToJSON, ToJSONKey)
import qualified Data.Aeson as AE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (listDirectory)

data DB :: Type -> Type -> Type where
  Users :: FilePath -> DB Username User
  Messages :: Dir -> DB (Username, TimeCreated) Message

all :: (Ord k) => DB k v -> IO (Either String (Map k v))
all db =
  case db of
    Users file ->
      AE.eitherDecodeFileStrict file :: IO (Either String (Map Username User))
    Messages dir -> do
      messages <- 
        fmap sequence
        $ traverse
           (\path -> do
               contents <- sequence <$> AE.eitherDecodeFileStrict path :: IO (Either String (Map TimeCreated Message))
               pure (Username $ T.pack path, sequence contents))
        =<< listDirectory (show dir)
      pure $ _

get :: (Ord k, FromJSONKey k, FromJSON v) =>
  k -> DB k v -> IO (Either String v)
get key db =
  case db of
    Users file -> do
      users <- AE.eitherDecodeFileStrict file
        :: IO (Either String (Map Username User))
      pure $ maybe (Left "not found") Right =<< Map.lookup key <$> users
    Messages dir -> do
      messages <- AE.eitherDecodeFileStrict (messagesFile dir (fst key))
        :: IO (Either String (Map TimeCreated Message))
      pure $ maybe (Left "not found") Right =<< Map.lookup (snd key) <$> messages

insert :: (Ord k, ToJSONKey k, ToJSON v) =>
  k -> v -> DB k v -> IO (Either String v)
insert key value db =
  case db of
    Users file -> do
      users <- AE.eitherDecodeFileStrict file
        :: IO (Either String (Map Username User))
      res <- traverse (AE.encodeFile file) $ Map.insert key value <$> users
      pure $ const value <$> res
    Messages dir ->
      let file = messagesFile dir (fst key)
      in do
        messages <- AE.eitherDecodeFileStrict file
            :: IO (Either String (Map TimeCreated Message))
        res <- traverse (AE.encodeFile file) $ Map.insert (snd key) value <$> messages
        pure $ const value <$> res

messagesFile :: Dir -> Username -> FilePath
messagesFile dir username =
  show dir <> "/" <> show username
  
