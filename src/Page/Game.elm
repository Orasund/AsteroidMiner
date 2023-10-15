module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Action exposing (Action)
import Color
import Config exposing (size, spriteSize, winAt)
import Data.Map exposing (SquareType(..))
import Html exposing (Html)
import Html.Attributes
import Layout
import PixelEngine exposing (Area)
import PixelEngine.Image as Image
import PixelEngine.Options as Options exposing (Options, Transition)
import Random exposing (Seed)
import View
import View.RunningGame as RunningGame exposing (Status(..))
import View.Tileset as Tileset


type alias Model =
    RunningGame.Model


type alias GameAction =
    Action Model Never Never ()


init : Seed -> ( Model, Cmd msg )
init seed =
    ( RunningGame.init { map = Data.Map.init, seed = seed, winCondition = winAt }, Cmd.none )


type Msg
    = Exit
    | GameSpecific RunningGame.Msg


subscriptions : Model -> Sub Msg
subscriptions =
    RunningGame.subscriptions
        >> Sub.map GameSpecific


update : Msg -> Model -> GameAction
update msg model =
    case msg of
        Exit ->
            Action.exiting

        GameSpecific gameMsg ->
            case model.status of
                Running ->
                    model
                        |> RunningGame.update gameMsg
                        >> (\m -> Action.updating ( m, Cmd.none ))

                _ ->
                    Action.updating ( model, Cmd.none )


areas : Model -> Html Msg
areas ({ status } as model) =
    case status of
        Running ->
            [ RunningGame.gameArea [] model
            , RunningGame.guiArea model
                |> List.singleton
                |> PixelEngine.toHtml
                    { options = Just Config.defaultOptions
                    , width = (toFloat <| Config.size) * Config.spriteSize
                    }
            ]
                |> Layout.column []
                |> Html.map GameSpecific

        Won ->
            [ PixelEngine.imageArea
                { height = (toFloat <| size) * spriteSize
                , background =
                    PixelEngine.colorBackground <|
                        Color.rgb255 218 212 94
                }
                [ ( ( (toFloat <| (size // 2) - 4) * spriteSize, (toFloat <| size // 2) * spriteSize )
                  , Image.fromText "Game Won" Tileset.font
                        |> Image.clickable Exit
                  )
                ]
            ]
                |> PixelEngine.toHtml
                    { options = Just Config.defaultOptions
                    , width = (toFloat <| Config.size) * Config.spriteSize
                    }

        Lost ->
            [ PixelEngine.imageArea
                { height = (toFloat <| size) * spriteSize
                , background =
                    PixelEngine.colorBackground <|
                        Color.rgb255 20 12 28
                }
                [ ( ( (toFloat <| (size // 2) - 4) * spriteSize, (toFloat <| size // 2) * spriteSize )
                  , Image.fromText "Game Lost" Tileset.font
                        |> Image.clickable Exit
                  )
                ]
            ]
                |> PixelEngine.toHtml
                    { options = Just Config.defaultOptions
                    , width = (toFloat <| Config.size) * Config.spriteSize
                    }


view :
    (Msg -> msg)
    -> Model
    -> Html msg
view mapper model =
    [ model
        |> areas
        |> Html.map mapper
    ]
        |> Layout.column
            ([ Html.Attributes.style "background-color" "rgb(20, 12, 28)"
             , Html.Attributes.style "height" "100%"
             , Html.Attributes.style "gap" View.bigSpace
             ]
                ++ Layout.centered
            )
