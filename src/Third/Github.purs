module Third.Github (Query, Message, component) where

import CSS (value, marginTop)
import CSS.Flexbox (AlignSelfValue(..), alignSelf)
import CSS.Size (rem)
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as AXResponse
import Third.HTML as HTML
import Third.Style as S
import Web.UIEvent.KeyboardEvent as Key
import Prelude

newtype UserDetails = UserDetails String

type Response = Either ResponseFormatError UserDetails

type State = { username :: String
             , loading :: Boolean
             , response :: Maybe Response
             }

type Input = Unit

data Query a = SetUsername String a
             | Fetch a
             | GetResponse (Maybe Response -> a)

data Message = UsernameSet String
             | Fetched Response

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.component
    { initialState: const { username: "", loading: false, response: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }

render :: State -> H.ComponentHTML Query
render { username, loading, response } =
  HH.div [ style S.col ]
  [ HTML.input "username"
    [ HP.disabled loading
    , HP.value username
    , HE.onValueInput $ HE.input SetUsername
    , HE.onKeyUp
      $ (\k -> if Key.code k == "Enter"
               then HE.input_ Fetch unit
               else Nothing
        )
    ]
  , HTML.btn
    [ style do
         marginTop (rem 1.0)
         alignSelf $ AlignSelfValue $ value "center"
    , HP.disabled loading
    , HE.onClick $ HE.input_ Fetch
    ]
    [ HH.text "fetch"
    ]
  , case response of
      Nothing ->
        HH.div [ style S.paragraph ]
        [ HH.text "search for a username to get something here"
        ]
      Just (Left e) ->
        HH.pre_
        [ HH.code [ style S.code ]
          [ HH.text $ AXResponse.printResponseFormatError e
          ]
        ]
      Just (Right (UserDetails details)) ->
        HH.pre_
        [ HH.code [ style S.code ]
          [ HH.text $ details
          ]
        ]
    ]

eval :: Query ~> H.ComponentDSL State Query Message Aff
eval q =
  case q of
    SetUsername s next -> do
      H.modify_ (_ { username = s })
      H.raise $ UsernameSet s
      pure next
    Fetch next -> do
      state <- H.get
      H.modify_ (_ { loading = true })
      response <-
        H.liftAff
        $ AX.get AXResponse.string
        $ "https://api.github.com/users/" <> state.username
      H.modify_ (_ { loading = false
                   , response = Just $ UserDetails <$> response.body
                   }
                )
      H.raise $ Fetched $ UserDetails <$> response.body
      pure next
    GetResponse respond -> do
      state <- H.get
      pure $ respond state.response
