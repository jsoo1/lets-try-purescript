module Fourth.User (Query(..) , Message(..) , component) where

import Affjax as AX
import Affjax.RequestBody as AXRequest
import Affjax.ResponseFormat as AXResponse
import CSS (borderRadius, backgroundColor, color, display, flex, fontStyle, height, margin, paddingLeft, width, value)
import CSS.Color (grey, red)
import CSS.Flexbox (AlignItemsValue(..), alignItems, flexBasis, flexGrow, flexShrink)
import CSS.Font (italic)
import CSS.Size (pct, rem)
import CSS.TextAlign (textAlign, center)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Fourth.Data (Err, User(..), Username, decode)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML (ClassName(..))
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Third.HTML as HTML
import Third.Style as Style
import Prelude

-- | Now our parent will provide us with a username
-- | So we take this input when we start up
type Input = Either Retrieval User

-- | The parent might want to know when we fetched someone
data Message = Fetched (Either Err User)

data Query a = Fetch a

data State = Retrieving Retrieval
           | Finished (Either { username :: Username, err :: Err } User)

type Retrieval =
  { username :: Username
  , url :: String
  , content :: Maybe AXRequest.RequestBody
  }

-- | We now have a nontrivial initial state
-- | `receiver` will be called when we start up
component :: H.Component HH.HTML Query Input Message Aff
component =
  H.lifecycleComponent
  { initialState : either Retrieving (Finished <<< Right)
  , receiver : HE.input_ Fetch
  , render
  , eval
  , initializer : Just $ H.action Fetch
  , finalizer : Nothing
  }

render :: State -> H.ComponentHTML Query
render s =
  case s of
    Retrieving retrieval ->
      HH.div [ style Style.row ]
      [ HH.div
        [ style do
             borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
             backgroundColor grey
        ]
        []
      , HH.div [ style Style.paragraph ]
        [ HH.text $ show retrieval.username
        ]
      ]
    Finished (Right (User user)) ->
      HTML.a
      [ HP.href user.url 
      , style do
           Style.row
           Style.outlined
           display flex
           alignItems $ AlignItemsValue $ value "center"
           width (rem 36.0)
           margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
      ]
      [ case user.avatarUrl of
           Just url ->
             HH.img
             [ HP.src $ url
             , style do
                  height (rem 10.5)
                  width (rem 10.5)
                  borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
             ]
           Nothing ->
             HH.div
             [ style do
                  backgroundColor grey
                  height (rem 10.5)
                  width (rem 10.5)
                  borderRadius (pct 50.0) (pct 50.0) (pct 50.0) (pct 50.0)
             ]
             [ HH.text "" ]
      , HH.div
        [ HP.class_ $ ClassName "a"
        , style do
             flexBasis (pct 0.0)
             flexGrow 1
             flexShrink 1
             Style.col
             alignItems $ AlignItemsValue $ value "center"
             paddingLeft (rem 1.0)
        ]
        [ HH.div
          [ style do
               Style.paragraph
               fontStyle italic
          ]
          [ HH.text $ show user.username
          ]
        , HH.div [ style Style.paragraph ]
          [ HH.text $ maybe "no name" identity user.name
          ]
        , HH.div
          [ style do
               Style.caption
               textAlign center
          ]
          [ HH.text $ maybe "no bio" identity user.bio
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

-- | We fetch a user if we don't have one yet
eval :: Query ~> H.ComponentDSL State Query Message Aff
eval (Fetch next) = do
  s <- H.get
  case s of
    Finished _ -> pure next
    Retrieving retrieval -> do
        response <-
            H.liftAff
            $ AX.request
            $ AX.defaultRequest
            { method = Left GET
            , url = retrieval.url
            , content = retrieval.content
            , responseFormat = AXResponse.json
            }
        let res = decode response
        H.put
          $ Finished
          $ lmap (\err -> {username : retrieval.username , err}) res
        H.raise $ Fetched $ res
        pure next
