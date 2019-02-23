module Third.HTML (a, btn, input) where

import Prelude

import CSS (display, flex, value)
import CSS.Flexbox (AlignItemsValue(..), flexDirection, column, alignItems)
import DOM.HTML.Indexed (HTMLa, HTMLbutton, HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen.HTML as HH
import Halogen.HTML (ClassName(..))
import Halogen.HTML.CSS (style)
import Halogen.HTML.Elements (Node, Leaf)
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Third.Style as S

a :: forall p i. Node HTMLa p i
a properties =
  HH.a
  $ [ HP.class_ $ ClassName "a"
    , style S.anchor
    ]
  <> properties


btn :: forall p i. Node HTMLbutton p i
btn properties =
  HH.button $ [ class_ $ HH.ClassName "btn" ] <> properties

input :: forall p i. String -> Leaf HTMLinput p i
input label properties =
  HH.label []
    [ HH.div
      [ style do
           S.paragraph
           display flex
           flexDirection column
           alignItems $ AlignItemsValue $ value "center"
      ]
      [ HH.text label
      ]
    , HH.input
      $ [ HP.type_ InputText
        , style S.paragraph
        ]
      <> properties
    ]
