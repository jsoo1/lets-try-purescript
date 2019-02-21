module Fourth.Data (User(..), Username(..)) where

import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude

data User =
  User
  { username  :: Username
  , name      :: String
  , avatarUrl :: String
  , url       :: String
  , bio       :: String
  }

newtype Username = Username String

derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username
instance showUsername :: Show Username where
  show (Username s) = show s

instance decodeJsonUsername :: DecodeJson Username where
  decodeJson j =
    case toString j of
      Just u -> Right $ Username u
      Nothing -> Left "No Username found"

instance decodeJsonUser :: DecodeJson User where
  decodeJson j = do
    x <- decodeJson j
    username <- x .: "username"
    name <- x .: "name"
    avatarUrl <- x .: "avatarUrl"
    url <- x .: "url"
    bio <- x .: "bio"
    pure $ User { username, name, avatarUrl, url, bio }
