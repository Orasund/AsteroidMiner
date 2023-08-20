module View.GUI exposing (Model, Msg, init, select, toDefault, update, view)

import Data exposing (floorCosts, maxValue, mineVolume, size, spriteSize)
import Data.Item exposing (Item)
import Data.ToolSelection as ToolSelection exposing (BeltColor(..), ToolSelection(..))
import Location exposing (Location)
import PixelEngine.Image as Image exposing (Image)
import View
import View.Inventory as Inventory
import View.Map as Map
import View.Tileset exposing (font)
import View.Tileset.Big as Tileset


type alias Model =
    { selected : ToolSelection
    }


type Msg
    = ItemSelected ToolSelection


init : Model
init =
    { selected = Bag Nothing
    }


select : ToolSelection -> Model -> Model
select tool model =
    { model | selected = tool }


toDefault : Model -> Model
toDefault =
    always init


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemSelected blueprint ->
            { model | selected = blueprint }


viewList : List (Image msg) -> List ( Location, Image msg )
viewList list =
    let
        length : Float
        length =
            list
                |> List.length
                |> toFloat

        center : Float
        center =
            toFloat size * spriteSize / 2
    in
    list
        |> List.indexedMap
            (\i image ->
                ( ( center
                        - (length * spriteSize)
                        + (toFloat i * spriteSize * 2)
                  , 0
                  )
                , image
                )
            )


viewBlueprint : ToolSelection -> ToolSelection -> Image Msg
viewBlueprint selected blueprint =
    let
        { image, symobl } =
            case blueprint of
                ToolSelection.Mine ->
                    Tileset.mine

                ToolSelection.ConveyorBelt _ ->
                    Tileset.conveyorBelt

                ToolSelection.Container ->
                    Tileset.container

                ToolSelection.Delete ->
                    Tileset.delete

                ToolSelection.Bag bag ->
                    bag
                        |> Maybe.map Map.viewItem
                        |> Tileset.pickUp

                ToolSelection.Merger ->
                    Tileset.merger

                ToolSelection.Sorter ->
                    Tileset.sorter

                ToolSelection.Floor ->
                    Tileset.floor
    in
    if blueprint == selected then
        image

    else
        symobl
            |> Image.clickable (ItemSelected blueprint)


viewDesc : ToolSelection -> List ( Location, Image Msg )
viewDesc selected =
    let
        text : String
        text =
            case selected of
                ToolSelection.Mine ->
                    "Mine - Mines " ++ String.fromInt mineVolume ++ " items"

                ToolSelection.ConveyorBelt Red ->
                    "Conveyor Belt (Red) - Transports items"

                ToolSelection.ConveyorBelt Blue ->
                    "Conveyor Belt (Blue) - Transports items"

                ToolSelection.ConveyorBelt Green ->
                    "Conveyor Belt (Green) - Transports items"

                ToolSelection.ConveyorBelt Yellow ->
                    "Conveyor Belt (Yellow) - Transports items"

                ToolSelection.Container ->
                    "Container - Stores " ++ String.fromInt maxValue ++ " items"

                ToolSelection.Delete ->
                    "DELETE BUILDINGS"

                ToolSelection.Bag _ ->
                    "PICK UP ITEMS"

                ToolSelection.Merger ->
                    "Merger - Takes from containers"

                ToolSelection.Sorter ->
                    "Sorter - Sorts into containers"

                ToolSelection.Floor ->
                    "Floor - Costs " ++ String.fromInt floorCosts ++ " items"
    in
    [ ( ( 0, (toFloat <| 2) * spriteSize ), Image.fromText text font ) ]


view : Maybe Item -> Int -> Model -> List ( Location, Image Msg )
view bag inventory { selected } =
    List.concat
        [ [ Bag bag
          , Mine
          , ConveyorBelt Red
          , ConveyorBelt Blue
          , Container
          , Merger
          , Sorter
          , Floor
          , Delete
          ]
            |> List.map (viewBlueprint selected)
            |> viewList
        , viewDesc selected
        , Inventory.view inventory
        ]
