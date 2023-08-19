module Building.ColoredConveyorBelt exposing (canStore, update)

import Building exposing (BeltColor, BuildingType(..), Code(..))
import Data.Item exposing (Item)
import Data.Map exposing (Command, Neighborhood)
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
                    |> Neighborhood.filter
                        (\a ->
                            (a == (ConveyorBelt <| Try color))
                                || (a == (ConveyorBelt <| Failed color))
                        )
                , neigh
                    |> Neighborhood.filter
                        (Building.isConveyorBeltColored color)
                ]

        outputs : List ( Direction, Maybe BuildingType )
        outputs =
            neigh
                |> Neighborhood.filter Building.isOutput

        inputs : List ( Direction, Maybe BuildingType )
        inputs =
            neigh
                |> Neighborhood.filter Building.isInput

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