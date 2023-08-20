module Data.Map exposing (Command, GroundType(..), Map, Neighborhood, Square, SquareType, init, update)

import Building exposing (BuildingType(..), Volume(..))
import Data exposing (size)
import Data.Item exposing (Item)
import Grid.Bordered as Grid
import Lib.Command as Command
import Lib.Map as Map exposing (SquareType(..))
import Lib.Neighborhood as Neighborhood


type GroundType
    = Dirt
    | Mountain { big : Bool }
    | OreGround


type alias Neighborhood =
    Neighborhood.Neighborhood (Maybe BuildingType)


type alias Command =
    Command.Command BuildingType Item


type alias SquareType =
    Map.SquareType BuildingType GroundType


type alias Square =
    Map.Square BuildingType GroundType Item


type alias Map =
    Map.Map BuildingType GroundType Item


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
                ( GroundSquare <| Mountain { big = True }, Nothing ) |> Just

            else if (x + 1 - center) ^ 2 + (y - 1 - center) ^ 2 <= 3 ^ 2 then
                ( GroundSquare <| Mountain { big = False }, Nothing ) |> Just

            else if (x - center) ^ 2 + (y - center) ^ 2 <= 4 ^ 2 then
                ( GroundSquare <| Dirt, Nothing ) |> Just

            else
                Nothing
        )
        { rows = size
        , columns = size
        }


update :
    { empty : GroundType
    , update : ( Int, Int ) -> Command
    , canStore : ( Int, Int ) -> BuildingType -> Item -> { value : Int, item : Item } -> Bool
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
                            |> (case maybeItem of
                                    Just _ ->
                                        case sort of
                                            Container Empty ->
                                                identity

                                            Container _ ->
                                                (+) (1 + value)

                                            _ ->
                                                identity

                                    _ ->
                                        identity
                               )
                        )

                    _ ->
                        ( m
                        , inv
                        )
            )
            ( map, 0 )
