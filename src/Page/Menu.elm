module Page.Menu exposing (Model, Msg(..), init, update, view)

import Action exposing (Action)
import Color
import Data exposing (size, spriteSize, version)
import Html exposing (Html)
import Page exposing (GameMode(..))
import PixelEngine exposing (Area)
import PixelEngine.Image as Image
import PixelEngine.Options exposing (Options)
import PixelEngine.Tile as Tile
import Random exposing (Seed)
import View.Tileset exposing (font)
import View.Tileset.Big as Tileset


type alias Model =
    Seed


type alias MenuAction =
    Action Never Never GameMode Never


type Msg
    = GameStarted
    | TutorialStarted


init : Seed -> ( Model, Cmd msg )
init seed =
    ( seed, Cmd.none )


update : Msg -> Model -> MenuAction
update msg model =
    case msg of
        GameStarted ->
            Action.transitioning (Game model)

        TutorialStarted ->
            Action.transitioning (Tutorial model)


areas : Model -> List (Area Msg)
areas _ =
    [ PixelEngine.imageArea
        { height = (toFloat <| size) * spriteSize
        , background =
            PixelEngine.colorBackground <|
                Color.rgb255 20 12 28
        }
        [ ( ( spriteSize * 8, spriteSize * 1 )
          , Image.fromTile
                (Tile.fromPosition ( 0, 0 )
                    |> Tile.animated 2
                )
                { source = "logo.png", spriteHeight = 128, spriteWidth = 128 }
          )
        , ( ( spriteSize * 15, spriteSize * 20 )
          , Tileset.gameMenuButton |> Image.clickable GameStarted
          )
        , ( ( spriteSize * 6, spriteSize * 20.5 )
          , Image.fromText "New Game" font |> Image.clickable GameStarted
          )
        , ( ( spriteSize * 15, spriteSize * 23 )
          , Tileset.tutorialMenuButton |> Image.clickable TutorialStarted
          )
        , ( ( spriteSize * 18, spriteSize * 23.5 )
          , Image.fromText "Tutorial" font |> Image.clickable TutorialStarted
          )
        , ( ( 0, 0 )
          , Image.fromText version font
          )
        ]
    ]


view :
    Options msg
    -> Model
    -> { options : Options msg, body : List (Area Msg) }
view options model =
    { options = options
    , body = model |> areas
    }
