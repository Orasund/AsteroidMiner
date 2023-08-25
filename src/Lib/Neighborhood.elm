module Lib.Neighborhood exposing (Neighborhood, fromPosition)

import Direction exposing (Direction(..))
import Grid.Bordered as Grid exposing (Grid)
import Position


type alias Neighborhood a =
    List ( Direction, a )


fromPosition : ( Int, Int ) -> Grid a -> ( Maybe a, List ( Direction, Maybe a ) )
fromPosition pos grid =
    let
        get : Direction -> Maybe a
        get direction =
            grid
                |> Grid.get (direction |> Direction.toCoord |> Position.addTo pos)
                |> Result.withDefault Nothing
    in
    grid
        |> Grid.get pos
        |> Result.andThen
            (\a ->
                Ok
                    ( a
                    , [ ( Up, get Up )
                      , ( Left, get Left )
                      , ( Right, get Right )
                      , ( Down, get Down )
                      ]
                    )
            )
        |> Result.toMaybe
        |> Maybe.withDefault ( Nothing, [] )
