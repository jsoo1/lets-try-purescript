{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeInType            #-}

module DB (DB(..), KV, all, delete, get, insert) where

import           Control.Concurrent.Async    (async, wait)
import           Control.Concurrent.MVarLock (Lock, newLock, withLock)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, readTVar)
import           Control.Monad.STM           (atomically)
import           Data                        (Dir, Message, TimeCreated, User,
                                              Username (..), by)
import           Data.Aeson                  (FromJSON, FromJSONKey, ToJSON,
                                              ToJSONKey)
import qualified Data.Aeson                  as AE
import           Data.Aeson.Text             (encodeToLazyText)
import           Data.Kind                   (Type)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as T
import           Prelude                     hiding (all)
import           System.Directory            (doesFileExist, listDirectory)

data DB :: Type -> Type -> Type where
  Users :: FilePath -> Lock -> DB Username User
  Messages :: Dir -> (TVar (Map Username Lock)) -> DB (Username, TimeCreated) Message

class (Ord k, FromJSONKey k, ToJSONKey k) => Key k where
class (FromJSON v, ToJSON v) => Value v where
class (Key k, Value v) => KV k v where

all :: KV k v => DB k v -> IO (Either String (Map k v))
all db =
  case db of
    Users file lock -> readUsersFile file lock
    Messages dir lock -> do
      messages <- sequence <$> (traverse (readMessagesFile' dir lock . username')
             =<< listDirectory (show dir))
      pure $ fmap (\fs -> Map.fromList $ (\(t, m) -> ((by m, t), m)) <$> (Map.toList =<< fs)) messages

get :: KV k v => DB k v -> k -> IO (Either String (Maybe v))
get db key =
  case db of
    Users file lock -> do
      users <- readUsersFile file lock
      pure $ Map.lookup key <$> users
    Messages dir lock -> do
      messages <- readMessagesFile' dir lock (fst key)
      pure $ Map.lookup (snd key) <$> messages

insert :: KV k v => DB k v -> k -> v -> IO (Either String v)
insert db key value =
  case db of
    Users file lock -> wait =<< async (withLock lock $ do
      users <- wait =<< async (readUsersFile' file)
      res <- wait =<< async (traverse (AE.encodeFile file) $ Map.insert key value <$> users)
      pure $ const value <$> res)
    Messages dir lock -> do
      l <- lockUserMessages lock (fst key)
      wait =<< async (withLock l $ do
        messages <- wait =<< async (readMessagesFile'' dir (fst key))
        res <- wait =<< async (traverse (writeMessagesFile dir (fst key))
            $ Map.insert (snd key) value <$> messages)
        pure $ const value <$> res)

delete :: KV k v => DB k v -> k -> IO (Either String ())
delete db key =
  case db of
    Users file lock -> wait =<< async (withLock lock $ do
      users <- wait =<< async (readUsersFile' file)
      wait =<< async (traverse (AE.encodeFile file) $ Map.delete key <$> users))
    Messages dir lock -> do
      l <- lockUserMessages lock (fst key)
      wait =<< async (withLock l $ do
        messages <- wait =<< async (readMessagesFile'' dir (fst key))
        wait =<< async (traverse (writeMessagesFile dir (fst key)) $ Map.delete (snd key) <$> messages))


-- Plumbing

instance KV Username User where
instance Key Username where
instance Value User where

instance KV (Username, TimeCreated) Message where
instance Key (Username, TimeCreated) where
instance Value Message where

readUsersFile :: FilePath -> Lock -> IO (Either String (Map Username User))
readUsersFile path lock =
  wait =<< async (withLock lock (wait =<< async (AE.eitherDecodeFileStrict path)))

readUsersFile' :: FilePath -> IO (Either String (Map Username User))
readUsersFile' = AE.eitherDecodeFileStrict

lockUserMessages :: TVar (Map Username Lock) -> Username -> IO Lock
lockUserMessages locks username = do
  lock <- newLock
  atomically $ modifyTVar locks (Map.alter (maybe (pure lock) pure) username)
  atomically $ pure . maybe lock id . Map.lookup username =<< readTVar locks

messagesFile :: Dir -> Username -> FilePath
messagesFile dir username = show dir <> "/" <> show username

readMessagesFile :: Dir -> Username -> Lock -> IO (Either String (Map TimeCreated Message))
readMessagesFile dir username lock =
  wait =<< (async $ withLock lock $ do
               fileP <- doesFileExist $ messagesFile dir username
               if fileP
                 then pure ()
                 else writeFile (messagesFile dir username) "[]"
               wait =<< async (readMessagesFile'' dir username))

readMessagesFile' :: Dir -> TVar (Map Username Lock) -> Username -> IO (Either String (Map TimeCreated Message))
readMessagesFile' dir lock username = readMessagesFile dir username =<< lockUserMessages lock username

readMessagesFile'' :: Dir -> Username -> IO (Either String (Map TimeCreated Message))
readMessagesFile'' dir username = do
  fileP <- doesFileExist $ messagesFile dir username
  if fileP
    then pure ()
    else writeFile (messagesFile dir username) "[]"
  AE.eitherDecodeFileStrict $ messagesFile dir username

writeMessagesFile :: Dir -> Username -> Map TimeCreated Message -> IO ()
writeMessagesFile dir username = AE.encodeFile $ messagesFile dir username

username' :: String -> Username
username' = Username . T.pack
