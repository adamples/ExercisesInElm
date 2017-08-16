import Html exposing (beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Dict
import Set exposing (Set)

import Css
import Css.Elements
import Css.Namespace

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Vector2d as Vector2d
-- import OpenSolid.Vector2d as Vector2d


styles =
    Css.asPairs >> Html.Attributes.style

main =
  beginnerProgram { model = model, view = view, update = update }


{- Model ------------------------------------------------------------------- -}

px: Float -> String
px x = toString x |> (++) "px"

point2dToString: Point2d -> String
point2dToString (Point2d (x, y)) = (toString x) ++ "," ++ (toString y)

vector2dToString: Vector2d -> String
vector2dToString (Vector2d (x, y)) = (toString x) ++ "," ++ (toString y)

type PathDataSegment
  = MoveTo Point2d
  | MoveRel Vector2d
  | LineTo Point2d
  | LineRel Vector2d
  | Close

type alias PathData = List PathDataSegment

moveTo: Point2d -> PathData -> PathData
moveTo point pathData = pathData ++ [MoveTo point]

startAt: Point2d -> PathData
startAt point = moveTo point []

moveRel: Vector2d -> PathData -> PathData
moveRel vector pathData = pathData ++ [MoveRel vector]

lineTo: Point2d -> PathData -> PathData
lineTo point pathData = pathData ++ [LineTo point]

lineRel: Vector2d -> PathData -> PathData
lineRel vector pathData = pathData ++ [LineRel vector]

close: PathData -> PathData
close pathData = pathData ++ [Close]

pathDataSegmentToString: PathDataSegment -> String
pathDataSegmentToString segment =
  case segment of
    MoveTo point -> "M " ++ (point2dToString point)
    MoveRel vector -> "m " ++ (vector2dToString vector)
    LineTo point -> "L " ++ (point2dToString point)
    LineRel vector -> "l " ++ (vector2dToString vector)
    Close -> "z"

pathDataToString: PathData -> String
pathDataToString pathData =
  pathData
  |> List.map pathDataSegmentToString
  |> String.join " "


type UnitTransform
  = Translate Vector2d
  | Rotate Float

unitTransformToString transform =
  case transform of
    Translate vector -> "translate(" ++ (vector2dToString vector) ++ ")"
    Rotate angle -> "rotate(" ++ (toString (angle * 180.0 / pi)) ++ ")"

type alias Transform = List UnitTransform

translate: Vector2d -> Transform -> Transform
translate vector transform = transform ++ [Translate vector]

translateTo: Point2d -> Transform -> Transform
translateTo point transform =
  let
    vector = Point2d.vectorFrom Point2d.origin point
  in
    transform ++ [Translate vector]

rotate: Float -> Transform -> Transform
rotate angle transform = transform ++ [Rotate angle]


transformToString transform =
  transform
  |> List.map unitTransformToString
  |> String.join " "


arrowShape: Float -> Float -> PathData
arrowShape length width =
  startAt Point2d.origin
  |> lineTo (Point2d (width / 2, -length))
  |> lineTo (Point2d (-width / 2, -length))
  |> close

arrow: Point2d -> Direction2d -> Float -> Float -> Svg a
arrow tip direction length width =
  let
    t = [] |> translateTo tip |> rotate (Direction2d.toAngle direction)
    pathData = arrowShape length width
  in
    Svg.path
    [ transform (transformToString t),
      d (pathDataToString pathData),
      fill "black",
      strokeWidth "0"
    ] []

lineBetween: Point2d -> Point2d -> Svg a
lineBetween p1 p2 =
  let
    pathData = startAt p1 |> lineTo p2
  in
    Svg.path
    [ d (pathDataToString pathData)
    , strokeWidth "1"
    , stroke "black"
    , fill "none"
    ]
    []

techText: String -> Point2d -> Svg msg
techText s (Point2d (px, py)) =
  Svg.text_ [ x (toString px), y (toString py), fontFamily "ISOCPEUR" ] [ Svg.text s ]

dimension: Point2d -> Point2d -> Svg msg
dimension a b =
  case Point2d.directionFrom a b of
    Just parallel ->
      let
        perpendicularDir = Direction2d.perpendicularTo parallel
        perpendicular1 = Direction2d.toVector perpendicularDir |> Vector2d.scaleBy 40
        perpendicular2 = Direction2d.toVector perpendicularDir |> Vector2d.scaleBy 50
        a1 = Point2d.translateBy perpendicular1 a
        b1 = Point2d.translateBy perpendicular1 b
        a2 = Point2d.translateBy perpendicular2 a
        b2 = Point2d.translateBy perpendicular2 b
      in
        g []
        [ arrow a1 perpendicularDir 12 4
        , arrow b1 (Direction2d.flip perpendicularDir) 12 4
        , lineBetween a a2
        , lineBetween b b2
        , lineBetween a1 b1
        , techText (toString (Point2d.distanceFrom a b)) (Point2d.interpolate a2 b2 0.5)
        ]
    Nothing -> g [] []


type Direction = North | East | South | West
type alias Position = { x: Float, y: Float }
type alias Orientation
  = {
    }

type alias Pin
  = { position: Position
    , direction: Direction
    , mirrored: Bool
    , length: Float
    }

renderPin: Pin -> Svg a
renderPin pin =
  g []
  [ Svg.path [ d ("M 0 0 L " ++ (toString pin.length) ++ " 0") ] []
  ]

type alias Path = { data: String }

renderPath: Path -> Svg a
renderPath path =
  Svg.path
    [ stroke "black"
    , strokeWidth "2px"
    , fill "none"
    , d path.data
    ]
    []

type SymbolPrimitive
  = SymbolPin Pin
  | SymbolPath Path

type alias Symbol = List SymbolPrimitive

renderSymbolPrimitive: SymbolPrimitive -> Svg a
renderSymbolPrimitive primitive =
  case primitive of
    SymbolPin pin -> renderPin pin
    SymbolPath path -> renderPath path

renderSymbol: Symbol -> Svg a
renderSymbol symbol =
  symbol |>
  List.map renderSymbolPrimitive |>
  g []

resistor1: Symbol
resistor1 =
  [ SymbolPath { data = "M -7.5 -20 l 15 0 l 0 40 l -15 0 z" }
  , SymbolPin { position = { x = 0, y = -30 }, length = 10, direction = North, mirrored = False }
  , SymbolPin { position = { x = 0, y = 50 }, length = 10, direction = South, mirrored = False }
  ]

type alias Rect = { x: Float, y: Float }
type alias Model = List Rect

model: Model
model = [ { x = 100, y = 200 }
        , { x = 300, y = 100}
        ]

{- end of Model ------------------------------------------------------------ -}

{- Message Type ------------------------------------------------------------ -}

type Msg = NOP

{- end of Message Type ----------------------------------------------------- -}

{- Update ------------------------------------------------------------------ -}

update: Msg -> Model -> Model
update msg model =
  case msg of
    NOP -> model

{- end of Update ----------------------------------------------------------- -}

{- View -------------------------------------------------------------------- -}

viewRect: Rect -> Svg Msg
viewRect r =
  rect [ width "100px"
       , height "100px"
       , x (toString r.x)
       , y (toString r.y)
       , fill "white"
       , strokeWidth "2px"
       , stroke "black"
       ] []

view: Model -> Html.Html Msg
view model =
  div []
  [ svg [ width "800px", height "600px", Html.Attributes.style [ ("background", "lightblue") ],
    transform "translate(0.5 0.5)", shapeRendering "geometricPrecision" ]
    [ g [ transform "translate(400, 300)" ]
      [ renderSymbol resistor1
      ],
      arrow (Point2d (200, 200)) (Direction2d.fromAngle 30) 20 4,
      dimension (Point2d (600, 400)) (Point2d (500, 300))
    ]
    --(List.map viewRect model)
  ]

{- end of View ------------------------------------------------------------- -}
