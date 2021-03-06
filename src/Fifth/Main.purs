module Fifth.Main (main) where

import Prelude

import Control.Coroutine (($$), (/\))
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Fifth.Data (Msg)
import Fifth.Page as Page
import Foreign (F, Foreign, unsafeToForeign, readString)
import Fourth.Data (User, Err(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver as Driver
import Web.Event.EventTarget as EET
import Web.HTML (window)
import Web.HTML.Location (host)
import Web.HTML.Window (location)
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

main :: Effect Unit
main = do
  host' <- host =<< location =<< window
  usersConnection <- WS.create ("ws://" <> host' <> "/user/subscribe" ) []
  messagesConnection <- WS.create ("ws://" <> host' <> "/message/subscribe") []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- Driver.runUI Page.component unit body

    -- | Send messages into our app
    CR.runProcess (wsProducer usersConnection /\ wsProducer messagesConnection $$ wsConsumer io.query)

    -- | We could send messages back, too:
    -- io.subscribe $ wsSender connection

-- | These websocket examples are taken straight from the halogen examples repo
-- | A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- | A consumer coroutine that takes the `query` function from our component IO
-- | record and sends `ReceiveMessage` queries in when it receives inputs from the
-- | producer.
wsConsumer :: (Page.Query ~> Aff) -> CR.Consumer (Tuple String String) Aff Unit
wsConsumer query = CR.consumer \(Tuple user msg) -> do
  void $ query $ H.action $ Page.NewUser $ decodeUser user 
  void $ query $ H.action $ Page.NewMessage $ decodeMessage msg
  pure Nothing
    where
      decodeUser :: String -> Either Err User
      decodeUser s = lmap JSONErr <<< decodeJson =<< lmap JSONErr (jsonParser s)

      decodeMessage :: String -> Either Err Msg
      decodeMessage s = lmap JSONErr <<< decodeJson =<< lmap JSONErr (jsonParser s)

wsMessage :: (Page.Query ~> Aff) -> CR.Consumer String Aff Unit
wsMessage query = CR.consumer \msg -> do
  void $ query $ H.action $ Page.NewMessage $ decode msg
  pure Nothing
    where
      decode :: String -> Either Err Msg
      decode s = lmap JSONErr <<< decodeJson =<< lmap JSONErr (jsonParser s)

-- | Should sending a message be desired
-- wsSender :: WS.WebSocket -> CR.Consumer Page.Message Aff Unit
-- wsSender socket = CR.consumer \msg -> do
--   case msg of
--     Page.SomeMessage msgContents ->
--       liftEffect $ WS.sendString socket msgContents
--     pure Nothing
