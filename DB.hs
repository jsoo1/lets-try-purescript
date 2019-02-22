{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TupleSections #-}

module DB (DB(..), all, delete, get, insert) where

import Prelude hiding (all)
import Data (Dir, Message, Username(..), TimeCreated, User, by)
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

class (Ord k, FromJSONKey k, ToJSONKey k) => Key k where
class (FromJSON v, ToJSON v) => Value v where
class (Key k, Value v) => KV k v where

all :: KV k v => DB k v -> IO (Either String (Map k v))
all db =
  case db of
    Users file -> readUsersFile file
    Messages dir -> do
      messages <- sequence <$> (traverse (readMessagesFile dir . username') =<< listDirectory (show dir))
      pure $ fmap (\fs -> Map.fromList $ (\(t, m) -> ((by m, t), m)) <$> (Map.toList =<< fs)) messages

get :: KV k v => DB k v -> k -> IO (Either String v)
get db key =
  case db of
    Users file -> do
      users <- readUsersFile file
      pure $ maybe (Left "not found") Right =<< Map.lookup key <$> users
    Messages dir -> do
      messages <- readMessagesFile dir (fst key)
      pure $ maybe (Left "not found") Right =<< Map.lookup (snd key) <$> messages

insert :: KV k v => DB k v -> k -> v -> IO (Either String v)
insert db key value =
  case db of
    Users file -> do
      users <- readUsersFile file
      res <- traverse (AE.encodeFile file) $ Map.insert key value <$> users
      pure $ const value <$> res
    Messages dir -> do
        messages <- readMessagesFile dir (fst key)
        res <- traverse (writeMessagesFile dir (fst key)) $ Map.insert (snd key) value <$> messages
        pure $ const value <$> res

delete :: KV k v => DB k v -> k -> IO (Either String ())
delete db key =
  case db of
    Users file -> do
      users <- readUsersFile file
      traverse (writeUsersFile file) $ Map.delete key <$> users
    Messages dir -> do
      messages <- readMessagesFile dir (fst key)
      traverse (writeMessagesFile dir (fst key)) $ Map.delete (snd key) <$> messages


-- Plumbing


messagesFile :: Dir -> Username -> FilePath
messagesFile dir username = show dir <> "/" <> show username

readUsersFile :: FilePath -> IO (Either String (Map Username User))
readUsersFile = AE.eitherDecodeFileStrict

writeUsersFile :: FilePath -> Map Username User -> IO ()
writeUsersFile p = AE.encodeFile p

readMessagesFile :: Dir -> Username -> IO (Either String (Map TimeCreated Message))
readMessagesFile dir = AE.eitherDecodeFileStrict . messagesFile dir 

writeMessagesFile :: Dir -> Username -> Map TimeCreated Message -> IO ()
writeMessagesFile dir username = AE.encodeFile $ messagesFile dir username

username' :: String -> Username
username' = Username . T.pack
