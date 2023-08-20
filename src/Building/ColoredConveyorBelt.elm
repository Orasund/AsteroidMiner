module Building.ColoredConveyorBelt exposing (canStore, update)

import Building exposing (BuildingType(..), Code(..))
import Data.Item exposing (Item)
import Data.Map exposing (Command, Neighborhood)
import Data.ToolSelection exposing (BeltColor(..))
import Direction exposing (Direction(..))
import Lib.Command as Command
import Lib.Neighborhood as Neighborhood


canStore : Neighborhood -> Item -> { value : Int, item : Item } -> Bool
canStore _ _ _ =
    False


areOpposing : List ( Direction, Maybe BuildingType ) -> List ( Direction, Maybe BuildingType ) -> Bool
areOpposing a b =
    case
        ( a
            |> List.head
            |> Maybe.map Tuple.first
        , b
            |> List.head
            |> Maybe.map Tuple.first
        )
    of
        ( Just Up, Just Down ) ->
            True

        ( Just Down, Just Up ) ->
            True

        ( Just Left, Just Right ) ->
            True

        ( Just Right, Just Left ) ->
            True

        _ ->
            False


update : BeltColor -> Direction -> Neighborhood -> Command
update color direction neigh =
    let
        resetCase : Command
        resetCase =
            Command.destroy

        friends : List ( Direction, Maybe BuildingType )
        friends =
            List.concat
                [ neigh
                    |> Neighborhood.toList
                    |> List.filterMap
                        (\( dir, ( maybeBuilding, _ ) ) ->
                            maybeBuilding
                                |> Maybe.andThen
                                    (\building ->
                                        if
                                            (building == (ConveyorBelt <| Try color))
                                                || (building == (ConveyorBelt <| Failed color))
                                        then
                                            ( dir, maybeBuilding ) |> Just

                                        else
                                            Nothing
                                    )
                        )
                , neigh
                    |> Neighborhood.toList
                    |> List.filterMap
                        (\( dir, ( maybeBuilding, _ ) ) ->
                            maybeBuilding
                                |> Maybe.andThen
                                    (\building ->
                                        if Building.isConveyorBeltColored color building then
                                            ( dir, maybeBuilding ) |> Just

                                        else
                                            Nothing
                                    )
                        )
                ]

        outputs : List ( Direction, Maybe BuildingType )
        outputs =
            neigh
                |> Neighborhood.toList
                |> List.filterMap
                    (\( dir, ( maybeBuilding, _ ) ) ->
                        maybeBuilding
                            |> Maybe.andThen
                                (\building ->
                                    if Building.isOutput building then
                                        Just ( dir, maybeBuilding )

                                    else
                                        Nothing
                                )
                    )

        inputs : List ( Direction, Maybe BuildingType )
        inputs =
            neigh
                |> Neighborhood.toList
                |> List.filterMap
                    (\( dir, ( maybeBuilding, _ ) ) ->
                        maybeBuilding
                            |> Maybe.andThen
                                (\building ->
                                    if Building.isInput building then
                                        Just ( dir, maybeBuilding )

                                    else
                                        Nothing
                                )
                    )

        isValid : Bool
        isValid =
            if
                (List.concat
                    [ friends
                    , outputs
                        |> List.head
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                    , inputs
                        |> List.head
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                    ]
                    |> List.length
                )
                    >= 2
            then
                case ( friends |> List.length, outputs |> List.length, inputs |> List.length ) of
                    ( 0, 1, 1 ) ->
                        areOpposing outputs inputs

                    ( _, 1, 1 ) ->
                        True

                    ( 2, _, _ ) ->
                        True

                    ( _, 1, _ ) ->
                        outputs
                            |> List.head
                            |> Maybe.map
                                (Tuple.first
                                    >> Direction.mirror
                                    >> (==) direction
                                )
                            |> Maybe.withDefault False

                    ( _, _, 1 ) ->
                        inputs
                            |> List.head
                            |> Maybe.map
                                (Tuple.first
                                    >> (==) direction
                                )
                            |> Maybe.withDefault False

                    _ ->
                        True

            else
                False
    in
    if isValid then
        Command.send direction

    else
        resetCase
