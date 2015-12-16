import Html exposing (div, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Css as Css exposing (Styles, style)
import Css.Dimension exposing (width, height)
import Css.Background as Background
import Css.Position as Position exposing (position, top, left)
import Color exposing (rgba)
import StartApp.Simple as StartApp
import Mouse as Mouse
import Signal exposing (map)
import Debug

main = map view model

model = Mouse.position

circleStyle: Int -> Int -> Styles
circleStyle x y = 
  position Position.Absolute
  <| top y
  <| left x
  <| width 40
  <| height 40
  <| Css.style "border-radius" "50%"
  <| Background.color (rgba 255 0 0 1)
  []

view: (Int, Int) -> Html.Html
view (x, y) =
  div []
    [ div [ Attr.style (circleStyle x y) ] [ text (toString (x, y)) ] ]
