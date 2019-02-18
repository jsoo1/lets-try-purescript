module First.Component
       ( Query(..)
       , State
       , Input
       , Message(..)
       , component
       , initialState
       , eval
       ) where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude

-- | Halogen uses a managed state approach
-- | Each render is a function of State
type State = Boolean

initialState :: State
initialState = false

-- | Halogen uses something like a free monad to describe your app
-- | Query is the "language" that you will later "interpret"
data Query a = Toggle a
             | WhatIsItNow (Boolean -> a)

-- | Create a halogen component
-- | Check out this beautiful record syntax!
component :: forall m . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }

-- | Turn your state into some v-dom
-- | Note that you can change which HTML renderer you might use!
-- | The Halogen.Html module provides nice functions render your DOM
-- | Users of elm will not feel out of place, here.
render :: State -> H.ComponentHTML Query
render s =
  HH.div []
    [ HH.div []
      [ HH.text $ show s ]
    , HH.button [ HE.onClick $ HE.input_ Toggle ]
        [ HH.text "toggle"
        ]
    ]

-- | Halogen uses a free monad to describe your app
-- | eval is how you interpret the language introduced by Query
-- | Think update for elm or reducers for redux - but in reverse
eval :: forall m . Query ~> H.ComponentDSL State Query Message m
eval q =
  case q of
    Toggle next -> do
      state <- H.get
      H.put $ not state
      H.raise $ Toggled $ not state
      pure next
    WhatIsItNow respond -> do
      state <- H.get
      pure $ respond state

-- | Ignore Input and Message for now
type Input = Unit
data Message = Toggled Boolean
