module Lib.Neighborhood exposing (Neighborhood, fromPosition, map, toList)

import Direction exposing (Direction(..))
import Grid.Bordered as Grid exposing (Error, Grid)
import Position


type alias Neighborhood a =
    { up : a
    , left : a
    , right : a
    , down : a
    }


toList : Neighborhood a -> List ( Direction, a )
toList { up, left, right, down } =
    [ ( Up, up ), ( Left, left ), ( Right, right ), ( Down, down ) ]


fromPosition : ( Int, Int ) -> Grid a -> Result Error ( Maybe a, Neighborhood (Maybe a) )
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
                    , { up = get Up
                      , left = get Left
                      , right = get Right
                      , down = get Down
                      }
                    )
            )


map : (a -> b) -> Neighborhood a -> Neighborhood b
map fun { up, left, right, down } =
    { up = fun up
    , left = fun left
    , right = fun right
    , down = fun down
    }
