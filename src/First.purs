module First
       ( Query(..)
       , Message(..)
       , component)
       where

import CSS (CSS, StyleM)
import CSS as CSS
import CSS.Font as Font
import CSS.Geometry (lineHeight)
import CSS.Size (px)
import Data.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (rel, href)
import Prelude

type State = Boolean

initialState :: State
initialState = false

data Query a = Toggle a
             | WhatIsItNow (Boolean -> a)

type Input = Unit

data Message = Toggled Boolean

component :: forall m . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }

render :: State -> H.ComponentHTML Query
render s =
  HH.div
  [ style normalFont ]
    [ HH.div [ style do Font.color $ if s then CSS.blue else CSS.red ]
      [ HH.text $ show s ]
    , HH.button [ HE.onClick $ HE.input_ Toggle ]
        [ HH.text "toggle"
        ]
    ]

normalFont :: StyleM Unit
normalFont = do
  lineHeight (px 24.0)
  Font.fontSize (px 24.0)
  cmuSerif

cmuSerif :: CSS
cmuSerif =
  Font.fontFamily [ "CMUSerifRoman" ]
    $ NonEmpty.singleton Font.sansSerif

eval :: forall m . Query ~> H.ComponentDSL State Query Message m
eval q =
  case q of
    Toggle x -> do
      state <- H.get
      H.put $ not state
      pure x
    WhatIsItNow respond -> do
      state <- H.get
      pure $ respond state
