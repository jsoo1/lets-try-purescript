{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data (Dir(..), Message, TimeCreated(..), Username(..), User, by, username) where

import           Data.Aeson            (FromJSON, FromJSONKey, ToJSON,
                                        ToJSONKey)
import           Data.Monoid           (Monoid)
import           Data.Semigroup        (Semigroup)
import           Data.Text
import           Data.Time.Clock.POSIX (POSIXTime)
import           Options.Generic       (Generic)

data User =
  User
  { username  :: Username
  , name      :: Text
  , avatarUrl :: Maybe Text
  , url       :: Text
  , bio       :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype Username = Username Text
  deriving (Eq, Ord, Semigroup, Monoid, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Show Username where
  show (Username u) = unpack u

data Message =
  Message
  { by      :: Username
  , created :: TimeCreated
  , content :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype TimeCreated = TimeCreated POSIXTime
  deriving (Eq, Ord, Semigroup, Monoid, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

newtype Dir = Dir FilePath

instance Show Dir where
  show (Dir d) = d
