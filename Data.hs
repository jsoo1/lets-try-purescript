{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data (Dir, Message, TimeCreated, Username(..), User, by, created, username) where

import           Data.Aeson            (FromJSON, FromJSONKey, ToJSON,
                                        ToJSONKey)
import           Data.Text
import           Data.Time.Clock.POSIX (POSIXTime)
import           Options.Generic       (Generic)

data User =
  User
  { username  :: Username
  , name      :: Text
  , avatarUrl :: Text
  , url       :: Text
  , bio       :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype Username = Username Text
  deriving (Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Show Username where
  show (Username u) = unpack u

data Message =
  Message
  { created :: TimeCreated
  , by      :: Username
  , content :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype TimeCreated = TimeCreated POSIXTime
  deriving (Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

newtype Dir = Dir FilePath

instance Show Dir where
  show (Dir d) = d
