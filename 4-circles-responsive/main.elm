import Html exposing (div)
import Html.Attributes as Attr exposing (style)
import Css as Css exposing (Styles, style)
import Css.Dimension exposing (width, height)
import Css.Background as Background
import Css.Position as Position exposing (position, top, left)
import Color exposing (rgba)
import Mouse as Mouse
import Window as Window
import Signal exposing (map, map2)
import Debug

main = view

type alias Circle = (Int, Int, Int)

circleStyle: Int -> Int -> Int -> Styles
circleStyle x y r = 
  position Position.Absolute
  <| top y
  <| left x
  <| width (2 * r)
  <| height (2 * r)
  <| Css.style "border-radius" "50%"
  <| Css.style "transform" "translate(-50%, -50%)"
  <| Background.color (rgba 90 150 120 1)
  []

circleDiv: Circle -> Html.Html
circleDiv (x, y, r) = div [ Attr.style (circleStyle x y r) ] []

circlesDivs: List Circle -> List Html.Html
circlesDivs cs = List.map circleDiv cs

circles: Int -> Int -> Int -> Int -> List Circle
circles nx ny margin radius = 
  let fstCircle = (0, 0, radius)
      base = List.repeat nx fstCircle
      row = List.scanl (\_ (x, y, r) -> (x + margin + 2 * radius, y, r)) fstCircle base
      rows = List.repeat ny base
      shift = \(x, y, r) -> (x, y + margin + 2 * radius, r)
  in List.concat <| List.scanl (\_ c -> List.map shift c) row rows

circleNb: Int -> Int -> Int -> Int
circleNb radius margin dim =
  let w = toFloat <| 2 * radius + margin
      dc = (toFloat dim) / w
  in round dc

view: Signal Html.Html
view = 
  let radius = 20
      margin = 10
      n = circleNb radius margin
      nx = map n Window.width
      ny = map n Window.height
      cs = map2 (\x y -> circles x y margin radius) nx ny
      divs = map circlesDivs cs
  in map (div []) divs
