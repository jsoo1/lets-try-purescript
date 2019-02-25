module Fifth.Data (Msg(..), TimeCreated(..), by, created, unTimeCreated) where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject, toNumber)
import Data.Argonaut.Decode (class DecodeJson, (.:), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, (~>), (:=), encodeJson)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (note)
import Fourth.Data (Username, encodeUsername)

newtype TimeCreated = TimeCreated Instant

derive instance eqTimeCreated :: Eq TimeCreated
derive instance ordTimeCreated :: Ord TimeCreated

data Msg =
  Msg
  { by :: Username
  , created :: TimeCreated
  , content :: String
  }

instance decodeJsonTimeCreated :: DecodeJson TimeCreated where
  decodeJson j =
    TimeCreated
    <$> (note "no instant" <<< instant <<< Milliseconds
         =<< note "no number found" (toNumber j))

instance decodeJsonMsg :: DecodeJson Msg where
  decodeJson j = do
    x <- decodeJson j
    by <- x .: "by"
    created <- x .: "created"
    content <- x .: "content"
    pure $ Msg { by, created, content }

instance encodeJsonTimeCreated :: EncodeJson TimeCreated where
  encodeJson (TimeCreated i) =
    let (Milliseconds x) = unInstant i in encodeJson x

instance encodeJsonMsg :: EncodeJson Msg where
  encodeJson (Msg m) = do
    (Tuple "by" $ encodeUsername m.by)
    ~> "created" := m.created
    ~> "content" := m.content
    ~> jsonEmptyObject

by :: Msg -> Username
by (Msg m) = m.by

created :: Msg -> TimeCreated
created (Msg m) = m.created

unTimeCreated :: TimeCreated -> Instant
unTimeCreated (TimeCreated t) = t
