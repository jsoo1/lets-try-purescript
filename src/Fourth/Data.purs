module Fourth.Data (User(..), Username(..)) where

import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

data User =
  User
  { username  :: Username
  , name      :: String
  , avatarUrl :: String
  , url       :: String
  , bio       :: String
  }

-- | Newtypes!
newtype Username = Username String

-- | Named instances!
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username
instance showUsername :: Show Username where
  show (Username s) = s

-- | This is how json decoding looks, done manually
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

-- | But why do all that, when you can derive it?
derive instance genericRepUsername :: Generic Username _
derive instance genericRepUser :: Generic User _

instance encodeJsonUsername :: EncodeJson Username where
  encodeJson = genericEncodeJson 

instance encodeJsonUser :: EncodeJson User where
  encodeJson = genericEncodeJson
