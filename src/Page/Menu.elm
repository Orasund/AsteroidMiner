module Page.Menu exposing (Model, Msg(..), init, subscriptions, update, view)

import Action exposing (Action)
import Color
import Data exposing (size, spriteSize, version)
import Html exposing (Html)
import Html.Attributes
import Layout
import Page exposing (GameMode(..))
import PixelEngine exposing (Area)
import PixelEngine.Image as Image
import PixelEngine.Options exposing (Options)
import PixelEngine.Tile as Tile
import Random exposing (Seed)
import Time
import View
import View.Tileset exposing (font)
import View.Tileset.Big


type alias Model =
    { frame : Int
    , seed : Seed
    }


type alias MenuAction =
    Action Model Never GameMode Never


type Msg
    = GameStarted
    | TutorialStarted
    | FrameRequested


init : Seed -> ( Model, Cmd msg )
init seed =
    ( { seed = seed
      , frame = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> MenuAction
update msg model =
    case msg of
        GameStarted ->
            Action.transitioning (Game model.seed)

        TutorialStarted ->
            Action.transitioning (Tutorial model.seed)

        FrameRequested ->
            Action.updating
                ( { model
                    | frame =
                        model.frame + 1 |> modBy 2
                  }
                , Cmd.none
                )


subscriptions : Sub Msg
subscriptions =
    Time.every 500 (\_ -> FrameRequested)


view : (Msg -> msg) -> Options msg -> Model -> Html msg
view mapper options model =
    [ "Asteroid Miner" |> Layout.text [ Html.Attributes.style "font-size" "2rem" ]
    , View.sprite []
        { spriteHeight = 128
        , spriteWidth = 128
        , sprite = ( model.frame, 0 )
        , url = "logo.png"
        , scale = 2
        }
    , [ [ View.Tileset.Big.gameMenuButton
        , "New Game" |> Layout.text []
        ]
            |> Layout.row [ Html.Attributes.style "gap" View.smallSpace ]
            |> View.button []
                { label = "New Game"
                , onPress = Just (mapper GameStarted)
                }

      {--, [ View.Tileset.Big.tutorialMenuButton
        , "Tutorial" |> Layout.text []
        ]
            |> Layout.row [ Html.Attributes.style "gap" View.smallSpace ]
            |> View.button []
                { label = "Tutorial"
                , onPress = Just (mapper TutorialStarted)
                }--}
      ]
        |> Layout.column [ Html.Attributes.style "gap" View.space ]
    ]
        |> Layout.column
            ([ Html.Attributes.style "background-color" "rgb(20, 12, 28)"
             , Html.Attributes.style "height" "100%"
             , Html.Attributes.style "gap" View.bigSpace
             ]
                ++ Layout.centered
            )
