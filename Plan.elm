module Plan exposing (..)

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d
import IntDict exposing (IntDict)
import Heap exposing (Heap, smallest, biggest, by)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgUtils exposing (..)
import Dict


type alias Wall =
    Float


type alias Corner =
    Point2d


type alias CornerId =
    Int


type alias Structure =
    { corners : IntDict Corner
    , walls : IntDict (IntDict Wall)
    }


emptyStructure =
    { corners = IntDict.empty
    , walls = IntDict.empty
    }


getNextId : IntDict a -> Int
getNextId dict =
    IntDict.findMax dict
        |> Maybe.map Tuple.first
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


addCorner : Point2d -> Structure -> ( CornerId, Structure )
addCorner corner { corners, walls } =
    let
        cornerId =
            getNextId corners

        nextStructure =
            { corners = corners |> IntDict.insert cornerId corner
            , walls = walls |> IntDict.insert cornerId IntDict.empty
            }
    in
        ( cornerId, nextStructure )


addHalfWall : Int -> Int -> Float -> Structure -> Structure
addHalfWall from to width { corners, walls } =
    let
        wall =
            width

        nextWalls =
            walls |> IntDict.update from (Maybe.map (IntDict.insert to wall))
    in
        { corners = corners
        , walls = nextWalls
        }


addWall : Int -> Int -> Float -> Float -> Structure -> Structure
addWall from to forwardWidth backwardWidth structure =
    structure
        |> addHalfWall from to forwardWidth
        |> addHalfWall to from backwardWidth



{-
   wallAngle : Structure -> Wall -> Float
   wallAngle { corners } { from, to } =
       let
           fromCorner =
               IntDict.get from corners |> Maybe.withDefault Point2d.origin

           toCorner =
               IntDict.get to corners |> Maybe.withDefault Point2d.origin
       in
           Point2d.directionFrom fromCorner toCorner
               |> Maybe.map Direction2d.toAngle
               |> Maybe.withDefault 0


   oppositeWall : Structure -> Wall -> Maybe Wall
   oppositeWall { walls } { from, to } =
       walls
           |> IntDict.get to
           |> Maybe.andThen (IntDict.get from)

-}


wrapList : List a -> List a
wrapList list =
    case list of
        [] ->
            []

        head :: tail ->
            tail ++ [ head ]


allValid : List (Maybe a) -> Maybe (List a)
allValid list =
    let
        list2 =
            List.filterMap identity list
    in
        if List.length list == List.length list2 then
            Just list2
        else
            Nothing


getWall : CornerId -> CornerId -> IntDict (IntDict Wall) -> Maybe Wall
getWall from to walls =
    IntDict.get from walls
        |> Maybe.andThen (IntDict.get to)


wallAngle : Corner -> Corner -> Float
wallAngle from to =
    Point2d.directionFrom from to
        |> Maybe.map Direction2d.toAngle
        |> Maybe.withDefault 0


type alias RoomWall =
    ( Wall, Point2d )


getCornerWalls : Structure -> ( CornerId, Corner ) -> Maybe (List ( RoomWall, RoomWall ))
getCornerWalls { corners, walls } ( cornerId, corner ) =
    let
        cornerWallsDict =
            IntDict.get cornerId walls

        cornerWalls =
            Maybe.map IntDict.values cornerWallsDict

        cornerNeighborIds =
            cornerWallsDict
                |> Maybe.map IntDict.keys

        cornerNeighbors =
            cornerNeighborIds
                |> Maybe.map (List.map (\cid -> IntDict.get cid corners))
                |> Maybe.andThen allValid

        oppositeWalls =
            cornerNeighborIds
                |> Maybe.map (List.map (\nid -> getWall nid cornerId walls))
                |> Maybe.andThen allValid
    in
        Maybe.map3 (List.map3 (\cw cn ow -> ( ( cw, cn ), ( ow, corner ) ))) cornerWalls cornerNeighbors oppositeWalls


sortCornerWalls : List ( RoomWall, RoomWall ) -> List ( RoomWall, RoomWall )
sortCornerWalls walls =
    List.sortBy (\( ( _, to ), ( _, from ) ) -> wallAngle from to) walls


cornerWallsSuccessors : List ( RoomWall, RoomWall ) -> List ( RoomWall, RoomWall )
cornerWallsSuccessors walls =
    let
        ( outgoing, ingoing ) =
            List.unzip walls

        shiftedIngoing =
            wrapList ingoing
    in
        List.map2 (,) outgoing shiftedIngoing


successorsForCorner : Structure -> ( CornerId, Corner ) -> Maybe (List ( RoomWall, RoomWall ))
successorsForCorner structure ( cornerId, corner ) =
    let
        walls =
            getCornerWalls structure ( cornerId, corner )
                |> Maybe.map sortCornerWalls
                |> Maybe.map cornerWallsSuccessors
    in
        walls


computeSuccessors : Structure -> Maybe (List ( RoomWall, RoomWall ))
computeSuccessors structure =
    IntDict.toList structure.corners
        |> List.map (successorsForCorner structure)
        |> allValid
        |> Maybe.map List.concat


type alias Room =
    List Point2d


structureFromGraph : List Point2d -> List ( Int, Int, Float, Float ) -> Structure
structureFromGraph points walls =
    let
        withNodes =
            List.foldl (\o m -> addCorner o m |> Tuple.second) emptyStructure points

        withWalls =
            walls
                |> List.foldl (\( from, to, w1, w2 ) -> addWall from to w1 w2) withNodes
    in
        withWalls


examplePlan : Structure
examplePlan =
    structureFromGraph
        [ Point2d ( 100, 100 )
        , Point2d ( 300, 100 )
        , Point2d ( 300, 200 )
        , Point2d ( 100, 200 )
        , Point2d ( 200, 200 )
        , Point2d ( 400, 200 )
        , Point2d ( 400, 400 )
        , Point2d ( 200, 400 )
        ]
        [ ( 0, 1, 10.0, 10.0 )
        , ( 1, 2, 10.0, 10.0 )
        , ( 2, 4, 10.0, 10.0 )
        , ( 4, 3, 10.0, 10.0 )
        , ( 3, 0, 10.0, 10.0 )
        , ( 2, 5, 10.0, 10.0 )
        , ( 5, 6, 10.0, 10.0 )
        , ( 6, 7, 10.0, 10.0 )
        , ( 7, 3, 10.0, 10.0 )
        ]


x =
    Debug.log "x" (computeSuccessors examplePlan)


circleAt : Float -> Point2d -> Svg msg
circleAt radius (Point2d ( x, y )) =
    circle
        [ cx (toString x)
        , cy (toString y)
        , r (toString radius)
        ]
        []


planToSvg : Structure -> Svg msg
planToSvg { corners, walls } =
    let
        points =
            IntDict.values corners
                |> List.map (circleAt 2)

        nodeLines =
            \node ->
                node.edges
                    |> List.map .node
    in
        g [] points


main : Svg msg
main =
    svg
        [ width "800px", height "800px" ]
        [ planToSvg examplePlan ]
