module Fourth.User (Err(..) , Query(..) , Message(..) , component) where

import Affjax as AX
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as AXResponse
import CSS (borderRadius, backgroundColor, color, fontStyle)
import CSS.Color (grey, red)
import CSS.Font (italic)
import CSS.Size (pct)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Fourth.Data (User(..), Username)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Third.Style as Style
import Prelude

-- | We can fail in two different ways
data Err = JSONErr String
         | ResponseErr ResponseFormatError

instance showErr :: Show Err where
  show e =
    case e of
      JSONErr s -> s
      ResponseErr r -> AXResponse.printResponseFormatError r

-- | Now our parent will provide us with a username
-- | So we take this input when we start up
type Input = Username

-- | The parent might want to know when we fetched someone
data Message = Fetched (Either Err User)

data Query a = Fetch a

data State = Retrieving Username
           | Finished (Either {username :: Username, err :: Err} User)

-- | We now have a nontrivial initial state
-- | `receiver` will be called when we start up
component :: H.Component HH.HTML Query Input Message Aff
component =
  H.component
  { initialState : Retrieving
  , receiver : HE.input_ Fetch
  , render
  , eval
  }

render :: State -> H.ComponentHTML Query
render s =
  HH.div [ style Style.col ]
  [ case s of
       Retrieving username ->
         HH.div [ style Style.row ]
         [ HH.div
           [ style do
                borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
                backgroundColor grey
           ]
           []
         , HH.div [ style Style.paragraph ]
           [ HH.text $ show username
           ]
         ]
       Finished (Right (User user)) ->
         HH.div [ style Style.row ]
         [ HH.a
           [ HP.href user.url
           , style do
                borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
                backgroundColor grey
           ]
           [ HH.img [ HP.src $ user.avatarUrl ]
           ]
         , HH.div [ style Style.col ]
           [ HH.div
             [ style do
                  Style.paragraph
                  fontStyle italic
             ]
             [ HH.text $ show user.username
             ]
           , HH.div [ style Style.paragraph ]
             [ HH.text user.name
             ]
           , HH.div [ style Style.caption ]
             [ HH.text user.bio
             ]
           ]
         ]
       Finished (Left {username, err}) ->
         HH.div [ style Style.row ]
         [ HH.div
           [ style do
                borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
                backgroundColor grey
           ]
           []
         , HH.div
           [ style do
                Style.col
                Style.paragraph
           ]
           [ HH.div_ [ HH.text $ show username ]
           , HH.div [ style $ color red ]
             [ HH.text $ show err
             ]
           ]
         ]
    ]


-- | We are always ready to fetch again, should the need arise
eval :: Query ~> H.ComponentDSL State Query Message Aff
eval (Fetch next) = do
  username <- getUsername <$> H.get
  response <-
    H.liftAff
    $ AX.get AXResponse.json
    $ "https://api.github.com/users/" <> show username
  let user = lmap JSONErr <<< decodeJson =<< lmap ResponseErr response.body
  H.put $ Finished $ lmap (\err -> {username, err}) user
  H.raise $ Fetched $ user
  pure next

    where
      getUsername :: State -> Username
      getUsername s =
        case s of
          Retrieving username -> username
          Finished (Left {username}) -> username
          Finished (Right (User u)) -> u.username
