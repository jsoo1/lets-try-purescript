module Fourth.Page (Query(..), component) where

import CSS (color, fontSize)
import CSS.Color (black, red)
import CSS.Size (px)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Either.Nested (type (\/))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Fourth.Data (Err, User, Username, username, decode, encodeUser, get, post)
import Fourth.Github as Github
import Fourth.User as User
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Prelude
import Third.Style as Style

data Query a = GetAll a
             | HandleUser Username User.Message a
             | HandleGithub Github.Message a

type State = { users :: Maybe (Either Err (Map Username User))
             , loading :: Boolean
             }

-- | If there are multiple children query types, you must say so
type ChildQuery = User.Query <\/> Github.Query <\/> Const Void
type ChildSlot = Username \/ Unit \/ Void

-- | There are corresponding constructors for parent components
-- | Note we are using the lifecycle to fetch our users
component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.lifecycleParentComponent
  { initialState : const { users : Nothing, loading : false }
  , render
  , eval
  , receiver : const Nothing
  , initializer : Just $ H.action GetAll
  , finalizer : Nothing
  }

-- | When rendering with multiple child types, you have to route them via `ChildPath`
-- | That is the CP.cpN functions below (up to 10 are defined, but you can make more)
render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
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
    [ HH.slot' CP.cp2 unit Github.component unit $ HE.input HandleGithub
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
             $ (pure (HH.slot' CP.cp1)
                <*> username
                <*> const User.small
                <*> pure <<< identity
                <*> HE.input <<< HandleUser <<< username)
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

eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
eval q =
  case q of
    GetAll next -> do
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ get "/user/all"
      H.modify_ (_ { loading = false
                   , users = Just $ decode response
                   })
      pure next
    HandleGithub (Github.Selected user) next -> do
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ post "/user" encodeUser user
      H.modify_ (\s ->
                  s { loading = false
                    , users =
                      pure $ pure (pure Map.insert <*> username <*> identity)
                      <*> decode response
                      <*> maybe (pure Map.empty) identity s.users
                    })
      pure next
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

