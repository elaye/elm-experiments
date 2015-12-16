import Html exposing (div, text)
import Mouse as Mouse
import Signal exposing (map)
import Debug

main: Signal Html.Html
main = map view model

model: Signal (Int, Int)
model = Mouse.position

view: (Int, Int) -> Html.Html
view pos = div [] [ text (toString pos) ]
