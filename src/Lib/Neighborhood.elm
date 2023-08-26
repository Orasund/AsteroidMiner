module Lib.Neighborhood exposing (Neighborhood, fromPosition)

import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Position


type alias Neighborhood a =
    List ( Direction, a )


fromPosition : ( Int, Int ) -> Dict ( Int, Int ) a -> ( Maybe a, List ( Direction, Maybe a ) )
fromPosition pos grid =
    let
        get : Direction -> Maybe a
        get direction =
            grid
                |> Dict.get (direction |> Direction.toCoord |> Position.addTo pos)
    in
    ( Dict.get pos grid
    , [ ( Up, get Up )
      , ( Left, get Left )
      , ( Right, get Right )
      , ( Down, get Down )
      ]
    )
