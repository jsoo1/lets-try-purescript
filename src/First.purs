module First
       ( Query(..)
       , Message(..)
       , component)
       where

import Data.Maybe
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
  HH.div []
    [ HH.div
      [ -- HP.style [ HP. ]
      ]
      [ HH.text $ if s then "true" else "false" ]
    , HH.button
        [ HE.onClick $ HE.input_ Toggle
        ]
        [ HH.text "toggle"
        ]
    ]

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
