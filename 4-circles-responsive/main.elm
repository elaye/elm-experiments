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
  <| Background.color (rgba 90 50 120 1)
  []

circleDiv: Circle -> Html.Html
circleDiv (x, y, r) = div [ Attr.style (circleStyle x y r) ] []

circlesDivs: Signal (List Circle) -> Signal (List Html.Html)
circlesDivs cs = map (List.map circleDiv) cs

circles: Signal Int -> Signal Int -> Int -> Int -> Signal (List Circle)
circles nx ny margin radius = 
  let fstCircle = (0, 0, radius)
      base = map (\n -> List.repeat n fstCircle) nx
      row = map (\b -> List.scanl (\_ (x, y, r) -> (x + margin + 2 * radius, y, r)) fstCircle b) base
      rows = map2 (\n b -> List.repeat n b) ny base
      shift = \(x, y, r) -> (x, y + margin + 2 * radius, r)
  in map2 (\rw rws -> List.concat <| List.scanl (\_ c -> List.map shift c) rw rws) row rows

circleNb: Int -> Int -> Signal Int -> Signal Int
circleNb radius margin dim =
  let w = toFloat <| 2 * radius + margin
      dc = map (\d -> (toFloat d) / w) dim
  in map (\n -> round n) dc

view: Signal Html.Html
view = 
  let radius = 20
      margin = 10
      n = circleNb radius margin
      nx = n Window.width
      ny = n Window.height
      divs = circlesDivs <| circles nx ny margin radius
  in map (div []) divs
