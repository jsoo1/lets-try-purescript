module Second.Component
       ( Query
       , Message
       , component
       ) where


import CSS (StyleM
           , display
           , height
           , justifyContent
           , flex
           , padding
           , paddingBottom
           , paddingTop
           )
import CSS as CSS
import CSS.Flexbox ( AlignItemsValue(..)
                   , AlignSelfValue(..)
                   , JustifyContentValue(..)
                   , alignItems
                   , alignSelf
                   , flexDirection
                   , flexBasis
                   , flexGrow
                   , flexShrink
                   , column
                   , row
                   )
import CSS.Font as Font
import CSS.Geometry (lineHeight)
import CSS.Property (value)
import CSS.Size (pct, px, rem, vh)
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NonEmpty
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_)
import Prelude


-- | Let's add some styling
-- | This is css and halogen-css
-- | They thought of most everything
-- | If you need an escape hatch (like for pseudo elements) you can use class_
render :: State -> H.ComponentHTML Query
render s =
  HH.div
    [ style do
         normalFont
         display flex
         flexDirection row
         alignItems $ AlignItemsValue $ value "center"
         height (vh 100.0)
    ]
    [ HH.div
        [ style do
            flexBasis (pct 0.0)
            flexGrow 1
            flexShrink 1
            padding (rem 1.25) (rem 1.25) (rem 1.25) (rem 1.25)
            display flex
            flexDirection column
        ]
        [ HH.div
            [ style do
                Font.color $ if s then CSS.black else CSS.red
                display flex
                paddingTop (rem 1.25)
                paddingBottom (rem 1.25)
                justifyContent $ JustifyContentValue $ value "space-around"
            ]
            [ HH.text $ show s
            ]
        , HH.button
            [ class_ $ HH.ClassName "btn"
            , style do alignSelf $ AlignSelfValue $ value "center"
            , HE.onClick $ HE.input_ Toggle ]
            [ HH.text "toggle"
            ]
        ]
    ]


-- | Make the font nicer
-- | StyleM is a nice styling dsl
-- | We used it above (style do ...)
normalFont :: StyleM Unit
normalFont = do
  lineHeight (px 24.0)
  Font.fontSize (px 24.0)
  Font.fontFamily [ "CMUSerifRoman" ]
    $ NonEmpty.singleton Font.sansSerif


-- Same as before


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


type Input = Unit
data Message = Toggled Boolean


type State = Boolean


initialState :: State
initialState = false


data Query a = Toggle a
             | WhatIsItNow (Boolean -> a)


component :: forall m . H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
