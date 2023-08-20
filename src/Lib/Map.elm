module Lib.Map exposing (Map, SingleCommand(..), Square, SquareType(..), apply)

import Building exposing (Building, BuildingType)
import Data exposing (maxValue)
import Direction exposing (Direction)
import Grid.Bordered as Grid exposing (Error(..), Grid)
import Position


type SingleCommand
    = Store
    | Send Direction
    | Create
    | Transition BuildingType
    | Destroy


type SquareType a b
    = GroundSquare b
    | BuildingSquare (Building a)


type alias Square a b =
    ( SquareType a b, Bool )


type alias Map a b =
    Grid (Square a b)


store : ( Int, Int ) -> Building BuildingType -> Map BuildingType b -> Result Error (Map BuildingType b)
store pos ({ value } as building) m =
    let
        maybeItem : Bool
        maybeItem =
            m
                |> Grid.get pos
                |> Result.toMaybe
                |> Maybe.andThen identity
                |> Maybe.map Tuple.second
                |> Maybe.withDefault False
    in
    m
        |> Grid.update pos
            (always <|
                if
                    (value < maxValue)
                        && (maybeItem /= False)
                then
                    Ok <|
                        Just <|
                            ( BuildingSquare { building | value = value + 1 }
                            , False
                            )

                else
                    Err ()
            )


send :
    ( Int, Int )
    -> Building a
    -> Bool
    ->
        { empty : b
        , lookUp : Map a b
        , canStore : ( Int, Int ) -> a -> { value : Int } -> Bool
        }
    -> Direction
    -> Map a b
    -> Result Error (Map a b)
send pos ({ value } as building) maybeItem { lookUp, canStore } direction m =
    let
        neighborPos : ( Int, Int )
        neighborPos =
            direction |> Direction.toCoord |> Position.addTo pos

        updateNeighbor : Bool -> Map a b -> Result Error (Map a b)
        updateNeighbor maybeC =
            Grid.update neighborPos
                (\maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare b, False ) ->
                            Ok <|
                                Just <|
                                    ( BuildingSquare b, maybeC )

                        _ ->
                            Err ()
                )

        solveConflict : Map a b -> Result Error (Map a b)
        solveConflict =
            Grid.update neighborPos
                (\maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare b, True ) ->
                            if canStore neighborPos b.sort { value = b.value } then
                                Ok <|
                                    Just <|
                                        ( BuildingSquare { b | value = b.value + 1 }, True )

                            else
                                Err ()

                        _ ->
                            Err ()
                )
    in
    if maybeItem then
        lookUp
            |> Grid.get neighborPos
            |> Result.andThen
                (\maybeEntry ->
                    m
                        |> (case maybeEntry of
                                Just ( BuildingSquare _, False ) ->
                                    updateNeighbor maybeItem

                                Just ( BuildingSquare _, True ) ->
                                    solveConflict

                                _ ->
                                    always <| Err NotSuccessful
                           )
                )
            |> Result.andThen
                (Grid.update pos <|
                    always <|
                        let
                            _ =
                                ( building.sort, maybeItem )
                        in
                        if value > 1 then
                            Ok <|
                                Just <|
                                    ( BuildingSquare { building | value = value - 1 }
                                    , maybeItem
                                    )

                        else
                            Ok <|
                                Just <|
                                    ( BuildingSquare { building | value = 0 }, False )
                )

    else
        Err NotSuccessful


apply :
    List SingleCommand
    -> ( Int, Int )
    -> Square BuildingType b
    ->
        { empty : b
        , lookUp : Map BuildingType b
        , canStore : ( Int, Int ) -> BuildingType -> { value : Int } -> Bool
        }
    -> Map BuildingType b
    -> Map BuildingType b
apply command pos ( squareType, maybeItem ) ({ empty } as config) map =
    let
        transition : Building BuildingType -> BuildingType -> Map BuildingType b -> Result Error (Map BuildingType b)
        transition building sort =
            Grid.update pos <|
                \maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare _, mI ) ->
                            Ok <|
                                Just <|
                                    ( BuildingSquare { building | sort = sort }
                                    , mI
                                    )

                        _ ->
                            Err ()

        create : Building BuildingType -> Map BuildingType b -> Result Error (Map BuildingType b)
        create building =
            Grid.update pos <|
                always <|
                    Ok <|
                        Just <|
                            ( BuildingSquare { building | value = 0 }
                            , True
                            )

        destroy : Building BuildingType -> Map BuildingType b -> Result Error (Map BuildingType b)
        destroy _ =
            Grid.update pos <|
                \maybeSquare ->
                    case maybeSquare of
                        Just ( BuildingSquare _, mI ) ->
                            Ok <|
                                Just <|
                                    ( GroundSquare empty
                                    , mI
                                    )

                        _ ->
                            Err ()
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
                                    Store ->
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
                            |> Result.toMaybe
                    )
                |> List.head
                |> Maybe.withDefault map
