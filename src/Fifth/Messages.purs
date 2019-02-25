module Fifth.Messages (Query(..), Message(..), component) where

import CSS (height, value, width, backgroundColor, marginLeft, height, paddingLeft, paddingBottom, paddingTop, width)
import CSS.Border (borderRadius)
import CSS.Color (grey)
import CSS.Flexbox (AlignItemsValue(..), JustifyContentValue(..), alignItems, justifyContent)
import CSS.Font (fontStyle, italic)
import CSS.Overflow (overflowY, scroll)
import CSS.Size (rem, pct, vh)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.DateTime.Instant as Instant
import Data.Formatter.DateTime (format, FormatterCommand(..))
import Data.List ((:), List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console as Console
import Effect.Now (now)
import Fifth.Data (Msg(..), TimeCreated(..), by, created, unTimeCreated)
import Fourth.Data (Username, User(..), get, post, decode, username)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Third.Style as Style
import Third.HTML as HTML
import Web.UIEvent.KeyboardEvent as Key

data Query a = GetAllMessages a
             | NewMessage Msg a
             | SetMessage String a
             | Send a

type Input = { session :: User, users :: Map Username User }

data Message = Sent Msg

type State =
  { message :: String
  , session :: User
  , sending :: Boolean
  , users :: Map Username User
  , messages :: Maybe (Map (Tuple Username TimeCreated) Msg)
  }

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.lifecycleComponent
  { initialState :
     (\i -> { message : ""
            , sending : false
            , messages : Nothing
            , session : i.session
            , users : i.users
            })
  , render
  , eval
  , receiver : const Nothing
  , initializer : Just $ H.action GetAllMessages
  , finalizer : Nothing
  }

render :: State -> H.ComponentHTML Query
render s =
  HH.div
  [ style do
       Style.col
       justifyContent $ JustifyContentValue $ value "flex-end"
       alignItems $ AlignItemsValue $ value "left"
  ]
  [ case s.messages of
       Just messages ->
         HH.div
         [ style do
              overflowY scroll
              height (vh 75.0)
         ]
         $ (pure message <*> (\m -> Map.lookup (by m) s.users) <*> identity)
         <$> (Array.sortWith created $ Array.fromFoldable messages)
       Nothing ->
         HH.div [ style Style.flexOne ]
         [ HH.h2 [ style Style.paragraph ]
           [ HH.text "no one has posted a message, yet"
           ]
         ]
  , HH.div
    [ style do
         Style.row
         height (rem 2.5)
         alignItems $ AlignItemsValue $ value "center"
    ]
    [ HTML.input "your message"
      [ HP.disabled $ s.sending
      , HP.value s.message
      , HE.onValueInput $ HE.input SetMessage
      , HE.onKeyUp
        (\k -> if Key.code k == "Enter"
               then HE.input_ Send unit
               else Nothing)
      ]
    , HTML.btn
      [ style do
           marginLeft (rem 1.0)
      , HP.disabled $ s.sending
      , HE.onClick $ HE.input_ Send
      ]
      [ HH.text "send"
      ]
    ]
  ]
  where
    message u (Msg m) =
      HH.div
      [ style do
           Style.caption
           Style.row
           alignItems $ AlignItemsValue $ value "center"
      ]
      [ HH.div [ style Style.col ]
        [ HH.div
          [ style do
               fontStyle italic
               paddingBottom (rem 0.5)
          ]
          [ HH.text $ maybe "no user" show $ pure <<< username =<< u
          ]
        , avatar $ (\(User x) -> x.avatarUrl) =<< u
        ]
      , HH.div
        [ style do
             paddingLeft (rem 1.25)
             Style.col
        ]
        [ HH.div [ style Style.caption ]
          [ HH.text $ format formatter $ Instant.toDateTime $ unTimeCreated m.created
          ]
        , HH.div [] [ HH.text m.content ]
        ]
      ]
    formatter = Hours12 : Placeholder ":" : MinutesTwoDigits : Placeholder ":" : SecondsTwoDigits : Placeholder " " : Meridiem : Nil
    avatar url =
      case url of
        Just u ->
          HH.img
          [ HP.src $ u
          , style do
               height (rem 3.0)
               width (rem 3.0)
               borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
          ]
        Nothing ->
          HH.div
          [ style do
               backgroundColor grey
               height (rem 3.0)
               width (rem 3.0)
               borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
          ]
          [ HH.text ""
          ]


eval :: Query ~> H.ComponentDSL State Query Message Aff
eval q =
  case q of
    GetAllMessages next -> do
      response <- H.liftAff $ get "http://localhost:8080/message/all"
      case decode response of
        Right msgs -> H.modify_ (_ { messages = pure msgs })
        Left e -> H.liftEffect $ Console.log $ show e
      pure next
    NewMessage msg next -> do
      H.modify_ (\s -> s { messages = pure $ insert msg s.messages })
      pure next
    SetMessage s next -> do
      H.modify_ (_ { message = s })
      pure next
    Send next -> do
      H.modify_ (_ { sending = true })
      s <- H.get
      now' <- H.liftEffect now
      response <-
        H.liftAff
        $ post "http://localhost:8080/message" encodeJson
        $ Msg { created : TimeCreated now'
              , by : username s.session
              , content : s.message
              }
      case decode response of
        Left e -> H.liftEffect $ Console.log $ show e
        Right msg -> do
          H.modify_ (\x -> x { sending = false
                             , messages = pure $ insert msg x.messages
                             })
          H.raise $ Sent msg
      pure next
    where
      insert :: Msg -> Maybe (Map (Tuple Username TimeCreated) Msg) -> Map (Tuple Username TimeCreated) Msg
      insert msg msgs = Map.insert (Tuple (by msg) (created msg)) msg
                        $ maybe Map.empty identity msgs
