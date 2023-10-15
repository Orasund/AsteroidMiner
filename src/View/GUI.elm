module View.GUI exposing (Model, Msg, init, select, toDefault, update, view)

import Color
import Config exposing (floorCosts, maxValue, mineVolume, size, spriteSize)
import Data.ToolSelection as ToolSelection exposing (ToolSelection(..))
import Html exposing (Html)
import Layout
import Location exposing (Location)
import PixelEngine
import PixelEngine.Image as Image exposing (Image)
import View
import View.Tileset.Big as Tileset


type alias Model =
    { selected : Maybe ToolSelection
    }


type Msg
    = Select (Maybe ToolSelection)


init : Model
init =
    { selected = Nothing
    }


select : ToolSelection -> Model -> Model
select tool model =
    { model | selected = Just tool }


toDefault : Model -> Model
toDefault =
    always init


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select blueprint ->
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


viewBlueprint : Maybe ToolSelection -> ToolSelection -> Image Msg
viewBlueprint selected blueprint =
    let
        { image, symobl } =
            case blueprint of
                ToolSelection.Mine ->
                    Tileset.mine

                ToolSelection.Pipe ->
                    Tileset.conveyorBelt

                ToolSelection.Container ->
                    Tileset.container

                ToolSelection.Delete ->
                    Tileset.delete

                ToolSelection.Merger ->
                    Tileset.merger

                ToolSelection.Sorter ->
                    Tileset.sorter

                ToolSelection.Floor ->
                    Tileset.floor
    in
    if Just blueprint == selected then
        image
            |> Image.clickable (Select Nothing)

    else
        symobl
            |> Image.clickable (Select (Just blueprint))


viewDesc : Maybe ToolSelection -> Html Msg
viewDesc selected =
    (case selected of
        Just ToolSelection.Mine ->
            "Mine - Mines " ++ String.fromInt mineVolume ++ " items"

        Just ToolSelection.Pipe ->
            "Pipe - Transports items"

        Just ToolSelection.Container ->
            "Container - Stores " ++ String.fromInt maxValue ++ " items"

        Just ToolSelection.Delete ->
            "DELETE BUILDINGS"

        Just ToolSelection.Merger ->
            "Merger - Takes from containers"

        Just ToolSelection.Sorter ->
            "Sorter - Sorts into containers"

        Just ToolSelection.Floor ->
            "Floor - Costs " ++ String.fromInt floorCosts ++ " items"

        Nothing ->
            ""
    )
        |> Layout.text []


view : Model -> Html Msg
view { selected } =
    [ [ Mine
      , Pipe
      , Container
      , Merger
      , Floor
      , Delete
      ]
        |> List.map (viewBlueprint selected)
        |> viewList
        |> PixelEngine.imageArea
            { height = 2 * spriteSize
            , background =
                PixelEngine.colorBackground <|
                    Color.rgb255 20 12 28
            }
        |> List.singleton
        |> PixelEngine.toHtml
            { options = Just Config.defaultOptions
            , width = (toFloat <| Config.size) * Config.spriteSize
            }
    , viewDesc selected
    ]
        |> Layout.column Layout.centered
