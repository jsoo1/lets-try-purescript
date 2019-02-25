module Fourth.Github (Query(..), Message(..), component) where

import CSS (color, marginBottom, marginTop, paddingBottom, value)
import CSS.Color (red)
import CSS.Flexbox (AlignSelfValue(..), alignSelf)
import CSS.Size (rem)
import Data.Either (Either(..))
import Data.Maybe as Maybe
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Fourth.Data (User, Username(..), Err, username)
import Fourth.User as User
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Third.HTML as HTML
import Third.Style as Style
import Web.UIEvent.KeyboardEvent as Key

data Query a = SetUsername String a
             | Fetch a
             | Select a
             | HandleUser Username User.Message a

data Message = Selected User

type Input = Unit

type State =
  { username :: String
  , fetching :: Github
  }

data Github = Fetching (Maybe String)
            | Selecting (Either Err User)
            | Finished User

isFetching :: Github -> Boolean
isFetching x =
  case x of
    Fetching y -> Maybe.isJust y
    _ -> false

-- | We provide a Slot type for each kind of child component we have
data Slot = UserSlot String
-- | Slot types must have instances for Eq and Ord
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.parentComponent
  { initialState : const { username : "", fetching : Fetching Nothing }
  , render
  , eval
  , receiver : const Nothing
  }

-- | Note the new type for HTML.
-- | You must specify the slot and child query types
render :: State -> H.ParentHTML Query User.Query Slot Aff
render s =
  HH.section
  [ style do
       Style.col
       paddingBottom (rem 1.25)
  ]
  [ HH.h2 [ style Style.paragraph ]
    [ HH.text "please introduce yourself"
    ]
  , HTML.input "your github username"
    [ HP.disabled $ isFetching s.fetching
    , HP.value s.username
    , HE.onValueInput $ HE.input SetUsername
    , HE.onKeyUp
      (\k -> if Key.code k == "Enter"
             then HE.input_ Fetch unit
             else Nothing)
    ]
  , HTML.btn
    [ style do
         marginTop (rem 1.0)
         marginBottom (rem 0.5)
         alignSelf $ AlignSelfValue $ value "center"
    , HP.disabled $ isFetching s.fetching
    , HE.onClick $ HE.input_ Fetch
    ]
    [ HH.text "search"
    ]
  , case s.fetching of
      Selecting (Right user) ->
        HH.div [ style Style.col ]
        [ HH.slot
          (UserSlot $ show $ username user)
          User.component
          (Right user)
          (HE.input $ HandleUser $ username user)
        , HH.h3 [ style Style.paragraph ]
          [ HH.text "if this is you, select yes to add yourself"
          ]
        , HTML.btn
          [ HE.onClick $ HE.input_ Select
          , style do
               marginTop (rem 1.0)
               marginBottom (rem 1.0)
               alignSelf $ AlignSelfValue $ value "center"
          ]
          [ HH.text "yes"
          ]
        ]
      Selecting (Left e) ->
        HH.div [ style Style.paragraph ]
        [ HH.div []
          [ HH.text "something went wrong:"
          ]
        , HH.div [ style $ color red ]
          [ HH.text $ show e
          ]
        ]
      Fetching (Just username) ->
        HH.slot
        (UserSlot $ show username)
        User.component
        (Left $ { username : Username username
                , url : "https://api.github.com/users/" <> username
                , content : Nothing
                })
        (HE.input (HandleUser $ Username s.username))
      Fetching Nothing ->
        HH.text ""
      Finished user ->
        HH.div [ style Style.paragraph]
        [ HH.text $ "ok " <> show (username user) <> ", you've been added"
        ]
  ]

eval :: Query ~> H.ParentDSL State Query User.Query Slot Message Aff
eval q =
  case q of
    SetUsername s next -> do
      H.modify_ (_ { username = s })
      pure next
    Fetch next -> do
      s <- H.get
      H.modify_ (_ { fetching = Fetching $ Just s.username })
      pure next
    HandleUser name (User.Fetched response) next -> do
      H.modify_ (_ { fetching = Selecting $ response })
      pure next
    Select next -> do
      s <- H.get
      case s.fetching of
        Selecting (Right user) -> do
          H.modify_ (_ { fetching = Finished user })
          H.raise $ Selected user
          pure next
        _ -> pure next
