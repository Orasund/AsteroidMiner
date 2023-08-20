module Building exposing (Building, BuildingType(..), GroundType(..), Volume(..), canBreak, isInput, isOutput)

import Data.ToolSelection exposing (ToolSelection(..))


type alias Building =
    { value : Int
    , sort : BuildingType
    }


type GroundType
    = Dirt
    | Mountain { big : Bool }


type Volume
    = Empty
    | HalfEmpty
    | HalfFull
    | Full


type BuildingType
    = Mine
    | Pipe
    | Container Volume
    | Merger
    | Sorter


isOutput : BuildingType -> Bool
isOutput sort =
    case sort of
        Mine ->
            True

        Merger ->
            True

        _ ->
            False


isInput : BuildingType -> Bool
isInput sort =
    case sort of
        Container _ ->
            True

        Sorter ->
            True

        _ ->
            False


canBreak : BuildingType -> Bool
canBreak sort =
    case sort of
        Container Empty ->
            True

        Container _ ->
            False

        Mine ->
            False

        _ ->
            True
