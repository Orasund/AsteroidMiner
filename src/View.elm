module View exposing (ToolSelection(..), image, pixelated, sprite)

import Data.Item exposing (Item)
import Html exposing (Attribute, Html)
import Html.Attributes


type ToolSelection
    = Mine
    | ConveyorBelt
    | Container
    | Merger
    | Sorter
    | Floor
    | Delete
    | Bag (Maybe Item)


pixelated : Html.Attribute msg
pixelated =
    Html.Attributes.style "image-rendering" "pixelated"


sprite :
    List (Attribute msg)
    ->
        { spriteWidth : Float
        , spriteHeight : Float
        , width : Float
        , height : Float
        , sprite : ( Int, Int )
        , url : String
        }
    -> Html msg
sprite attrs args =
    let
        ( x, y ) =
            args.sprite
    in
    Html.div
        ([ pixelated
         , Html.Attributes.style "background-position"
            ("-" ++ String.fromInt x ++ "px -" ++ String.fromInt y ++ "px")
         , Html.Attributes.style "background-image" args.url
         ]
            ++ attrs
        )
        []


image :
    List (Attribute msg)
    -> { url : String, width : Float, height : Float }
    -> Html msg
image attrs args =
    Html.img
        ([ pixelated
         , Html.Attributes.style "src" args.url
         , Html.Attributes.style "width" (String.fromFloat args.width ++ "px")
         , Html.Attributes.style "height" (String.fromFloat args.height ++ "px")
         ]
            ++ attrs
        )
        []
