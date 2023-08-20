module Data.Map exposing (Neighborhood, init, update)

import Building exposing (BuildingType(..), GroundType(..), Volume(..))
import Data exposing (size)
import Grid.Bordered as Grid
import Lib.Map as Map exposing (Map, SingleCommand, SquareType(..))
import Lib.Neighborhood as Neighborhood


type alias Neighborhood =
    Neighborhood.Neighborhood ( Maybe BuildingType, Bool )


init : Map
init =
    let
        center : Int
        center =
            size // 2
    in
    Grid.fill
        (\( x, y ) ->
            if (x + 1 - center) ^ 2 + (y - 1 - center) ^ 2 <= 1 ^ 2 then
                ( GroundSquare <| Mountain { big = True }, False ) |> Just

            else if (x + 1 - center) ^ 2 + (y - 1 - center) ^ 2 <= 3 ^ 2 then
                ( GroundSquare <| Mountain { big = False }, False ) |> Just

            else if (x - center) ^ 2 + (y - center) ^ 2 <= 4 ^ 2 then
                ( GroundSquare <| Dirt, False ) |> Just

            else
                Nothing
        )
        { rows = size
        , columns = size
        }


update :
    { empty : GroundType
    , update : ( Int, Int ) -> List SingleCommand
    , canStore : ( Int, Int ) -> BuildingType -> { value : Int } -> Bool
    }
    -> Map
    -> ( Map, Int )
update fun map =
    map
        |> Grid.foldl
            (\pos maybeSquare ( m, inv ) ->
                case maybeSquare of
                    Just (( BuildingSquare { sort, value }, maybeItem ) as square) ->
                        ( m |> Map.apply (fun.update pos) pos square { empty = fun.empty, lookUp = map, canStore = fun.canStore }
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
