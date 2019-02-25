module Fourth.Data (Err(..), User(..), Username(..), username, decode, encodeUsername, encodeUser, get, post) where

import Affjax as AX
import Affjax.RequestBody as AXBody
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as AXResponse
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, toString, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Prelude

-- | We can fail in two different ways
data Err = JSONErr String
         | ResponseErr ResponseFormatError

instance showErr :: Show Err where
  show e =
    case e of
      JSONErr s -> s
      ResponseErr r -> AXResponse.printResponseFormatError r

data User =
  User
  { username  :: Username
  , name      :: Maybe String
  , avatarUrl :: Maybe String
  , url       :: String
  , bio       :: Maybe String
  }

username :: User -> Username
username (User u) = u.username

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

instance decodeBackendJsonUser :: DecodeJson User where
  decodeJson j = do
    x <- decodeJson j
    backendUsername <- x .:? "username"
    githubUsername <- x .:? "login"
    username <-
      note "neither username nor login found"
      $ backendUsername <|> githubUsername
    name <- x .: "name"
    githubAvatarUrl <- x .:? "avatar_url"
    backendAvatarUrl <- x .:? "avatarUrl"
    avatarUrl <-
      note "neither avatar_url nor avatarUrl found"
      $ backendAvatarUrl <|> githubAvatarUrl
    backendUrl <- x .:? "url"
    githubUrl <- (x .:? "html_url") :: Either String (Maybe String)
    url <-
      note "neither html_url nor url found"
      -- take the github one first, since the github api also has a "url" field
      $ githubUrl <|> backendUrl
    bio <- x .: "bio"
    pure $ User { username, name, avatarUrl, url, bio }

-- | But why do all that, when you can derive generic and let argonaut do it for you?
derive instance genericRepUsername :: Generic Username _
derive instance genericRepUser :: Generic User _

instance encodeJsonUsername :: EncodeJson Username where
  encodeJson = genericEncodeJson

instance encodeJsonUser :: EncodeJson User where
  encodeJson = genericEncodeJson

decode ::
  forall a. DecodeJson a
  => (AX.Response (Either ResponseFormatError Json))
  -> Either Err a
decode res = lmap JSONErr <<< decodeJson =<< lmap ResponseErr res.body

-- | However, I found that encoding for Aeson was not right when derived
encodeUsername :: Username -> Json
encodeUsername (Username u) =
  encodeJson u
  
encodeUser :: User -> Json
encodeUser (User u) =
  (Tuple "username" $ encodeUsername u.username)
  ~> "name" := u.name
  ~> "avatarUrl" := u.avatarUrl
  ~> "url" := u.url
  ~> "bio" := u.bio
  ~> jsonEmptyObject

get :: String -> Aff (AX.Response (Either ResponseFormatError Json))
get = AX.get AXResponse.json

post :: forall a. String -> (a -> Json) -> a -> Aff (AX.Response (Either ResponseFormatError Json))
post url encodeJson = AX.post AXResponse.json url <<< AXBody.json <<< encodeJson
