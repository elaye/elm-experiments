import Html exposing (div, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Css as Css exposing (Styles, style)
import Css.Dimension exposing (width, height)
import Css.Background as Background
import Css.Position as Position exposing (position, top, left)
import Color exposing (Color, rgba)
import StartApp.Simple as StartApp
import Mouse as Mouse
import Random as Rand
import Window as Window
import Signal exposing (map, map3, constant)
import Debug

main = view

model = Mouse.position

seed: Rand.Seed
seed = Rand.initialSeed 1729

type alias Circle = (Int, Int, Int)

circleColor: Int -> Color
circleColor radius =
  let r = round <| 255 * (toFloat radius) / 80 + 30
      g = round <| 255 * (toFloat radius) / 60 + 10
      b = round <| 255 * (toFloat radius) / 80 + 25
  in rgba r g b 1

circleStyle: Int -> Int -> Int -> Styles
circleStyle x y r = 
  position Position.Absolute
  <| top y
  <| left x
  <| width r
  <| height r
  <| Css.style "transform" "translate(-50%, -50%)" 
  <| Css.style "border-radius" "50%"
  <| Background.color (circleColor r)
  []

circleDiv: Circle -> Html.Html
circleDiv (x, y, r) = div [ Attr.style (circleStyle x y r) ] []

circles: Int -> Int -> List Circle
circles n radius = 
  let fstCircle = (60, 60, radius)
      base = List.repeat n fstCircle
      row = List.scanl (\_ (x, y, r) -> (x + 120, y, r)) fstCircle base
      rows = List.repeat n base
      shift = \(x, y, r) -> (x, y + 120, r)
  in List.concat <| List.scanl (\_ c -> List.map shift c) row rows

view: Html.Html
view = 
  let baseRadiuses = List.repeat 10 5
      radiuses = List.scanl (\_ r -> r + 5) 5 baseRadiuses
      circleDivs = \r -> List.map circleDiv (circles 10 r)
  in div [] <| List.reverse <| List.concatMap circleDivs radiuses
