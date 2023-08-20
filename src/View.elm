module View exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Layout


stylesheet : Html msg
stylesheet =
    """
body,html{
    height: 100%;
    margin:0;
    font-family: sans-serif;
    color: white;

}

button:hover{
    filter:brightness(0.9);
}

button:focus{
    filter:brightness(0.7);
}
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []


button : List (Attribute msg) -> { label : String, onPress : Maybe msg } -> Html msg -> Html msg
button attrs args =
    Layout.button
        ([ Html.Attributes.style "border" "2px solid white"
         , Html.Attributes.style "background" "transparent"
         , Html.Attributes.style "color" "white"
         , Html.Attributes.style "padding" (smallSpace ++ " " ++ space)
         ]
            ++ attrs
        )
        args


smallSpace : String
smallSpace =
    "0.5rem"


space : String
space =
    "1rem"


bigSpace : String
bigSpace =
    "2rem"



-------------------------------------------------------
-- LIBRARY
-------------------------------------------------------


pixelated : Html.Attribute msg
pixelated =
    Html.Attributes.style "image-rendering" "pixelated"


sprite :
    List (Attribute msg)
    ->
        { spriteWidth : Float
        , spriteHeight : Float
        , sprite : ( Int, Int )
        , url : String
        , scale : Int
        }
    -> Html msg
sprite attrs args =
    let
        ( x, y ) =
            args.sprite

        left =
            args.spriteWidth * toFloat x

        top =
            args.spriteHeight * toFloat y

        height =
            args.spriteHeight * toFloat args.scale

        width =
            args.spriteWidth * toFloat args.scale
    in
    Html.div
        ([ pixelated
         , Html.Attributes.style "background-position"
            ("-" ++ String.fromFloat left ++ "px -" ++ String.fromFloat top ++ "px")
         , Html.Attributes.style "background-image" ("url(" ++ args.url ++ ")")
         , Html.Attributes.style "width" (String.fromFloat args.spriteWidth ++ "px")
         , Html.Attributes.style "height" (String.fromFloat args.spriteHeight ++ "px")
         ]
            ++ withScale args.scale
            ++ attrs
        )
        []
        |> List.singleton
        |> Html.div
            [ Html.Attributes.style "width" (String.fromFloat width ++ "px")
            , Html.Attributes.style "height" (String.fromFloat height ++ "px")
            ]


withScale : Int -> List (Attribute msg)
withScale amount =
    [ Html.Attributes.style "scale" (String.fromInt amount)
    , Html.Attributes.style "transform-origin" "top left"
    ]


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
