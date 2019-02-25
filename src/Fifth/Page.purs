module Fifth.Page (Query(..), component) where

import CSS (color, display, flex)
import CSS.Color (black, red)
import CSS.Flexbox (flexDirection, row)
import CSS.Font (fontSize)
import CSS.Overflow (overflowY, scroll)
import CSS.Size (px)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..), hush)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Either.Nested (type (\/))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Console as Console
import Fifth.Data (Msg)
import Fifth.Messages as Messages
import Fourth.Data (Err, User, Username, decode, encodeUser, get, post, username)
import Fourth.User as User
import Fourth.Github as Github
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Prelude
import Third.Style as Style

data Query a = GetAllUsers a
             | NewUser (Either Err User) a
             | NewMessage (Either Err Msg) a
             | HandleUser Username User.Message a
             | HandleGithub Github.Message a
             | HandleCurrentUser User.Message a
             | HandleMessages Messages.Message a

type State =
  { users :: Maybe (Either Err (Map Username User))
  , loadingUsers :: Boolean
  , session :: Maybe User
  }

type ChildQuery = User.Query <\/> Github.Query <\/> User.Query <\/> Messages.Query <\/> Const Void
type ChildSlot = Username \/ Unit \/ Username \/ Unit \/ Void

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.lifecycleParentComponent
  { initialState :
    const { users : Nothing
          , loadingUsers : false
          , session : Nothing
          }
  , render
  , eval
  , receiver : const Nothing
  , initializer : Just $ H.action $ GetAllUsers
  , finalizer : Nothing
  }

render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
render s =
  HH.div
  [ style do
       Style.row
       display flex
       flexDirection row
  ]
  [ HH.div []
    [ HH.div []
      [ case s.session of
           Just user ->
             HH.slot' CP.cp3 (username user) User.small (pure user)
             $ HE.input HandleCurrentUser
           Nothing ->
             HH.h2
             [ style do
                  Style.caption
                  fontSize (px 18.0)
             ]
             [ HH.text "let's try purescript!"
             ]
      ]
    , HH.hr [ style $ color black]
    , HH.div []
      [ HH.section []
        [ HH.h2 [ style Style.paragraph ]
          [ HH.text "who all is here:"
          ]
        , case s.users of
             Just (Right users) ->
               HH.div [ style $ overflowY scroll ] $
               (pure (HH.slot' CP.cp1)
                <*> username
                <*> const User.tiny
                <*> pure <<< identity
                <*> HE.input <<< HandleUser <<< username)
               <$> Array.fromFoldable users
             Just (Left e) ->
               HH.div [ style Style.paragraph ]
               [ HH.div []
                 [ HH.text "something went wrong:"
                 ]
               , HH.div [ style $ color red ]
                 [ HH.text $ show e
                 ]
               ]
             Nothing ->
               HH.p [ style Style.paragraph ]
               [ HH.text "no one is here, yet. add your github to the list!"
               ]
        ]
      ]
    ]
  , HH.div [ style Style.flexOne ]
    [ case s.session of
       Just user ->
         HH.slot'
         CP.cp4
         unit
         Messages.component
         { session : user, users : maybe Map.empty identity $ hush =<< s.users }
         (HE.input HandleMessages)
       Nothing ->
         HH.slot' CP.cp2 unit Github.component unit $ HE.input HandleGithub
    ]
  ]

eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
eval q =
  case q of
    GetAllUsers next -> do
      H.modify_ (_ { loadingUsers = true })
      response <- H.liftAff $ get "/user/all"
      H.modify_ (_ { loadingUsers = false
                   , users = Just $ decode response
                   })
      pure next
    NewMessage msg next -> do
      case msg of
        Left e -> H.liftEffect $ Console.log $ show e
        Right m -> void $ H.query' CP.cp4 unit $ H.action $ Messages.NewMessage m
      pure next
    NewUser message next -> do
      H.modify_ (\s -> s { users =
                       pure $ pure (pure Map.insert <*> username <*> identity)
                       <*> message
                       <*> maybe (pure Map.empty) identity s.users
                       })
      pure next
    HandleGithub (Github.Selected user) next -> do
      H.modify_ (_ { loadingUsers = true })
      response <- H.liftAff $ post "/user" encodeUser user
      H.modify_ (\s ->
                  s { loadingUsers = false
                    , users =
                      pure $ pure (pure Map.insert <*> username <*> identity)
                      <*> decode response
                      <*> maybe (pure Map.empty) identity s.users
                    })
      H.modify_ (_ { session = hush $ decode response })
      pure next
    HandleCurrentUser (User.Fetched response) next -> pure next
    HandleUser name (User.Fetched response) next -> do
      s <- H.get
      H.modify_ (_ { users = pure $
                        case s.users of
                          Nothing ->
                            pure (\u -> Map.insert name u Map.empty) <*> response
                          Just users ->
                            pure (Map.insert name) <*> response <*> users
                   })
      pure next
    HandleMessages (Messages.Sent m) next -> pure next
