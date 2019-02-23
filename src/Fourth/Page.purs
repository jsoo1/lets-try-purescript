module Fourth.Page (Query(..), Message, component) where

import Affjax as AX
import Affjax.RequestBody as AXBody
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as AXResponse
import CSS (alignSelf, color, fontSize, marginBottom, marginTop, paddingBottom, value)
import CSS.Color (black, red)
import CSS.Flexbox (AlignSelfValue(..))
import CSS.Size (px, rem)
import Data.Argonaut (Json)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Fourth.Data (Err, User, Username(..), username, decode, encodeUser)
import Fourth.User as User
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Third.Style as Style
import Third.HTML as HTML
import Web.UIEvent.KeyboardEvent as Key

data Query a = GetAll a
             | SetUsername String a
             | FetchGithubUser a
             | SelectGithubUser a
             | HandleBackendUser Username User.Message a
             | HandleGithubUser Username User.Message a

type State = { users :: Maybe (Either Err (Map Username User))
             , loading :: Boolean
             , username :: String
             , fetching :: Either (Maybe String) (Either Err User)
             }

type Input = Unit

type Message = Void

data Slot = BackendUserSlot Username | GithubUserSlot String

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.lifecycleParentComponent
  { initialState : const initialState
  , render
  , eval
  , receiver : const Nothing
  , initializer : Just $ H.action GetAll
  , finalizer : Nothing
  }
  where
    initialState =
      { users : Nothing
      , loading : false
      , username : ""
      , fetching : Left Nothing
      }

render :: State -> H.ParentHTML Query User.Query Slot Aff
render s =
  HH.section [ style Style.col ]
  [ HH.h1
    [ style do
         Style.paragraph
         fontSize (px 48.0)
    ]
    [ HH.text "let's try purescript!"
    ]
  , HH.div [ style Style.row ]
    [ HH.section
      [ style do
           Style.col
           paddingBottom (rem 1.25)
      ]
      [ HH.h2 [ style Style.paragraph ]
        [ HH.text "please introduce yourself"
        ]
      , HTML.input "your github username"
        [ HP.disabled $ either Maybe.isJust (const false) s.fetching
        , HP.value s.username
        , HE.onValueInput $ HE.input SetUsername
        , HE.onKeyUp
          (\k -> if Key.code k == "Enter"
                 then HE.input_ FetchGithubUser unit
                 else Nothing)
        ]
      , HTML.btn
        [ style do
             marginTop (rem 1.0)
             marginBottom (rem 0.5)
             alignSelf $ AlignSelfValue $ value "center"
        , HP.disabled $ either Maybe.isJust (const false) s.fetching
        , HE.onClick $ HE.input_ FetchGithubUser
        ]
        [ HH.text "search"
        ]
      , case s.fetching of
          Right (Right user) ->
            HH.div [ style Style.col ]
            [ HH.slot
              (GithubUserSlot $ show $ username user)
              User.component
              (Right user)
              (HE.input (HandleGithubUser $ username user))
            , HH.h3 [ style Style.paragraph ]
              [ HH.text "if this is you, select yes to add yourself"
              ]
            , HTML.btn
              [ HE.onClick $ HE.input_ SelectGithubUser
              , style do
                   marginTop (rem 1.0)
                   marginBottom (rem 1.0)
                   alignSelf $ AlignSelfValue $ value "center"
              ]
              [ HH.text "yes"
              ]
            ]
          Right (Left e) ->
            HH.div
            [ style Style.paragraph ]
            [ HH.div []
              [ HH.text "something went wrong:"
              ]
            , HH.div [ style $ color red ]
              [ HH.text $ show e
              ]
            ]
          Left (Just username) ->
            HH.slot
            (GithubUserSlot $ show username)
            User.component
            (Left $ { username : Username username
                    , url : "https://api.github.com/users/" <> username
                    , content : Nothing
                    })
            (HE.input (HandleGithubUser $ Username s.username))
          Left Nothing ->
            HH.text ""
      ]
    ]
  , HH.hr [ style $ color black]
  , HH.div [ style Style.row ]
    [ HH.section [ style Style.col ]
      [ HH.h2 [ style Style.paragraph ]
        [ HH.text "who all is here:"
        ]
      , case s.users of
           Just (Right users) ->
             HH.div []
             $ (pure HH.slot
                  <*> BackendUserSlot <<< username
                  <*> const User.component
                  <*> pure <<< identity
                  <*> HE.input <<< HandleBackendUser <<< username)
             <$> Array.fromFoldable users
           Just (Left e) ->
             HH.div
             [ style Style.paragraph ]
             [ HH.div []
               [ HH.text "something went wrong:"
               ]
             , HH.div [ style $ color red ]
               [ HH.text $ show e
               ]
             ]
           Nothing ->
             HH.p
             [ style Style.paragraph ]
             [ HH.text
               $ "no one is here, yet."
               <> " add your github to the list!"
             ]
      ]
    ]
  ]

eval :: Query ~> H.ParentDSL State Query User.Query Slot Message Aff
eval q =
  case q of
    GetAll next -> do
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ get "http://localhost:8080/user/all"
      H.modify_ (_ { loading = false
                   , users = Just $ decode response
                   })
      pure next
    SetUsername s next -> do
      H.modify_ (_ { username = s })
      pure next
    SelectGithubUser next -> do
      s <- H.get
      case s.fetching of
        Right (Right user) -> do
          H.modify_ (_ { loading = true })
          response <- H.liftAff $ post "http://localhost:8080/user" encodeUser user
          H.modify_ (_ { loading = false
                       , users = 
                         pure $ pure (pure Map.insert <*> username <*> identity)
                         <*> decode response
                         <*> maybe (pure Map.empty) identity s.users
                       })
          pure next
        _ -> pure next
    FetchGithubUser next -> do
      s <- H.get
      H.modify_ (_ { fetching = Left $ Just s.username })
      pure next
    HandleGithubUser name (User.Fetched response) next -> do
      H.modify_ (_ { fetching = Right $ response  })
      pure next
    HandleBackendUser name (User.Fetched response) next -> do
      s <- H.get
      H.modify_ (_ { users = pure $
                        case s.users of
                          Nothing ->
                            pure (\u -> Map.insert name u Map.empty) <*> response
                          Just users ->
                            pure (Map.insert name) <*> response <*> users
                   })
      pure next

get :: String -> Aff (AX.Response (Either ResponseFormatError Json))
get = AX.get AXResponse.json

post :: forall a. String -> (a -> Json) -> a -> Aff (AX.Response (Either ResponseFormatError Json))
post url encodeJson = AX.post AXResponse.json url <<< AXBody.json <<< encodeJson

