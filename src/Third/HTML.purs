module Third.HTML (btn, input) where


import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Elements (Node, Leaf)
import DOM.HTML.Indexed (HTMLbutton, HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(..))
import Prelude
import Third.Style as S


btn :: forall p i. Node HTMLbutton p i
btn properties =
  HH.button $ [ class_ $ HH.ClassName "btn" ] <> properties


input :: forall p i. String -> Leaf HTMLinput p i
input label properties =
  HH.label []
    [ HH.div [ style S.paragraph ]
      [ HH.text label
      ]
    , HH.input
      $ [ HP.type_ InputText
        , style S.paragraph
        ]
      <> properties
    ]
