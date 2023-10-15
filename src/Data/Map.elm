module Data.Map exposing (..)

import Building exposing (Building, BuildingType(..), GroundType(..), Volume(..))
import Config
import Dict exposing (Dict)
import Direction exposing (Direction)
import Grid.Bordered as Grid exposing (Error(..))
import Lib.Neighborhood as Neighborhood
import Position


type alias Neighborhood =
    Neighborhood.Neighborhood ( Maybe BuildingType, Bool )


type SingleCommand
    = Save
    | Send Direction
    | Create
    | Transition BuildingType
    | Destroy


type SquareType
    = GroundSquare GroundType
    | BuildingSquare Building


type alias Square =
    ( SquareType, Bool )


type alias Map =
    Dict ( Int, Int ) Square


init : Map
init =
    let
        center : Int
        center =
            Config.size // 2
    in
    List.range 0 (Config.size - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Config.size - 1)
                    |> List.map (Tuple.pair x)
            )
        |> List.filterMap
            (\( x, y ) ->
                if (x + 1 - center) ^ 2 + (y - 1 - center) ^ 2 <= 1 ^ 2 then
                    ( GroundSquare <| Mountain { big = True }, False )
                        |> Tuple.pair ( x, y )
                        |> Just

                else if (x + 1 - center) ^ 2 + (y - 1 - center) ^ 2 <= 3 ^ 2 then
                    ( GroundSquare <| Mountain { big = False }, False )
                        |> Tuple.pair ( x, y )
                        |> Just

                else if (x - center) ^ 2 + (y - center) ^ 2 <= 4 ^ 2 then
                    ( GroundSquare <| Dirt, False )
                        |> Tuple.pair ( x, y )
                        |> Just

                else
                    Nothing
            )
        |> Dict.fromList


update :
    { empty : GroundType
    , update : ( Int, Int ) -> List SingleCommand
    , canStore : ( Int, Int ) -> BuildingType -> { value : Int } -> Bool
    }
    -> Map
    -> ( Map, Int )
update fun map =
    List.range 0 (Config.size - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (Config.size - 1)
                    |> List.map (Tuple.pair x)
            )
        |> List.foldl
            (\pos ( m, inv ) ->
                case Dict.get pos map of
                    Just (( BuildingSquare { sort, value }, maybeItem ) as square) ->
                        ( m |> apply (fun.update pos) pos square { empty = fun.empty, lookUp = map, canStore = fun.canStore }
                        , inv
                            |> (if maybeItem then
                                    case sort of
                                        Container Empty ->
                                            identity

                                        Container _ ->
                                            (+) (1 + value)

                                        _ ->
                                            identity

                                else
                                    identity
                               )
                        )

                    _ ->
                        ( m
                        , inv
                        )
            )
            ( map, 0 )


store : ( Int, Int ) -> Building -> Map -> Maybe Map
store pos ({ value } as building) m =
    let
        maybeItem : Bool
        maybeItem =
            m
                |> Dict.get pos
                |> Maybe.map Tuple.second
                |> Maybe.withDefault False
    in
    if
        (value < Config.maxValue)
            && (maybeItem /= False)
    then
        m
            |> Dict.insert pos
                ( BuildingSquare { building | value = value + 1 }
                , False
                )
            |> Just

    else
        Nothing


send :
    ( Int, Int )
    -> Building
    -> Bool
    ->
        { empty : GroundType
        , lookUp : Map
        , canStore : ( Int, Int ) -> BuildingType -> { value : Int } -> Bool
        }
    -> Direction
    -> Map
    -> Maybe Map
send pos ({ value } as building) maybeItem { lookUp, canStore } direction m =
    let
        neighborPos : ( Int, Int )
        neighborPos =
            direction |> Direction.toCoord |> Position.addTo pos

        updateNeighbor : Map -> Maybe Map
        updateNeighbor map =
            case map |> Dict.get neighborPos of
                Just ( BuildingSquare b, False ) ->
                    map
                        |> Dict.insert neighborPos ( BuildingSquare b, True )
                        |> Just

                _ ->
                    Nothing

        solveConflict : Map -> Maybe Map
        solveConflict map =
            case Dict.get neighborPos map of
                Just ( BuildingSquare b, True ) ->
                    if canStore neighborPos b.sort { value = b.value } then
                        map
                            |> Dict.insert neighborPos
                                ( BuildingSquare { b | value = b.value + 1 }
                                , True
                                )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
    in
    if maybeItem then
        m
            |> (case Dict.get neighborPos lookUp of
                    Just ( BuildingSquare _, False ) ->
                        updateNeighbor

                    Just ( BuildingSquare _, True ) ->
                        solveConflict

                    _ ->
                        \_ -> Nothing
               )
            |> Maybe.map
                (Dict.insert pos ( BuildingSquare building, False ))

    else if value > 0 then
        m
            |> Dict.insert pos
                ( BuildingSquare { building | value = building.value - 1 }, True )
            |> Just

    else
        Nothing


apply :
    List SingleCommand
    -> ( Int, Int )
    -> Square
    ->
        { empty : GroundType
        , lookUp : Map
        , canStore : ( Int, Int ) -> BuildingType -> { value : Int } -> Bool
        }
    -> Map
    -> Map
apply command pos ( squareType, maybeItem ) ({ empty } as config) map =
    let
        transition : Building -> BuildingType -> Map -> Maybe Map
        transition building sort m =
            case m |> Dict.get pos of
                Just ( BuildingSquare _, mI ) ->
                    m
                        |> Dict.insert pos
                            ( BuildingSquare { building | sort = sort }
                            , mI
                            )
                        |> Just

                _ ->
                    Nothing

        create : Building -> Map -> Maybe Map
        create building m =
            m
                |> Dict.insert pos
                    ( BuildingSquare { building | value = 0 }
                    , True
                    )
                |> Just

        destroy : Building -> Map -> Maybe Map
        destroy _ m =
            case Dict.get pos m of
                Just ( BuildingSquare _, mI ) ->
                    m
                        |> Dict.insert pos
                            ( GroundSquare empty
                            , mI
                            )
                        |> Just

                _ ->
                    Nothing
    in
    case squareType of
        GroundSquare _ ->
            map

        BuildingSquare building ->
            command
                |> List.filterMap
                    (\c ->
                        map
                            |> (case c of
                                    Save ->
                                        store pos building

                                    Send direction ->
                                        send pos building maybeItem config direction

                                    Transition sort ->
                                        transition building sort

                                    Create ->
                                        create building

                                    Destroy ->
                                        destroy building
                               )
                    )
                |> List.head
                |> Maybe.withDefault map
