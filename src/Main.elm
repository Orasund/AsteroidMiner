module Main exposing (main)

import Action exposing (Action)
import Browser
import Building exposing (BuildingType(..))
import Data exposing (fps, size, spriteSize)
import Data.Map exposing (SquareType(..))
import Html exposing (Html)
import Html.Attributes
import Layout
import Location exposing (Angle(..))
import Page exposing (GameMode(..))
import Page.Game as Game
import Page.Menu as Menu
import Page.Tutorial as Tutorial
import PixelEngine exposing (Area, Input(..), PixelEngine, gameWithNoControls)
import PixelEngine.Options as Options exposing (Options)
import Random exposing (Seed)
import View



----------------------
-- Model
----------------------


type Model
    = Loading
    | Menu Menu.Model
    | Game Game.Model
    | Tutorial Tutorial.Model


type LoadingMsg
    = GotSeed Seed


type Msg
    = GameSpecific Game.Msg
    | TutorialSpecific Tutorial.Msg
    | MenuSpecific Menu.Msg
    | LoadingSpecific LoadingMsg


type alias LoadingAction =
    Action Never Never Seed Never



----------------------
-- Init
----------------------


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.independentSeed |> Random.generate (LoadingSpecific << GotSeed)
    )



----------------------
-- Update
----------------------


updateLoading : LoadingMsg -> LoadingAction
updateLoading msg =
    case msg of
        GotSeed seed ->
            Action.transitioning seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingSpecific loadingMsg, Loading ) ->
            updateLoading loadingMsg
                |> Action.config
                |> Action.withTransition Menu.init Menu never
                |> Action.apply

        ( MenuSpecific menuMsg, Menu menuModel ) ->
            Menu.update menuMsg menuModel
                |> Action.config
                |> Action.withUpdate Menu never
                |> Action.withCustomTransition
                    (\data ->
                        case data of
                            Page.Game seed ->
                                let
                                    ( m, _ ) =
                                        Game.init seed
                                in
                                ( Game m, Cmd.none )

                            Page.Tutorial seed ->
                                let
                                    ( m, _ ) =
                                        Tutorial.init 1 seed
                                in
                                ( Tutorial m, Cmd.none )
                    )
                |> Action.apply

        ( GameSpecific gameMsg, Game gameModel ) ->
            Game.update gameMsg gameModel
                |> Action.config
                |> Action.withExit (init ())
                |> Action.withUpdate Game never
                |> Action.apply

        ( TutorialSpecific tutorialMsg, Tutorial tutorialModel ) ->
            Tutorial.update tutorialMsg tutorialModel
                |> Action.config
                |> Action.withExit (init ())
                |> Action.withUpdate Tutorial never
                |> Action.apply

        _ ->
            ( model, Cmd.none )



----------------------
-- Subscriptions
----------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Menu _ ->
            Menu.subscriptions
                |> Sub.map MenuSpecific

        Game gameModel ->
            gameModel
                |> Game.subscriptions
                |> Sub.map GameSpecific

        Tutorial tutorialModel ->
            tutorialModel
                |> Tutorial.subscriptions
                |> Sub.map TutorialSpecific



----------------------
-- View
----------------------


view : Model -> Html Msg
view model =
    let
        defaultOptions : Options msg
        defaultOptions =
            Options.default
                |> Options.withMovementSpeed (1 / fps)
                |> Options.withScale 3

        map : (a -> Msg) -> List (Area a) -> List (Area Msg)
        map mapper o =
            o |> List.map (PixelEngine.mapArea mapper)

        body =
            case model of
                Loading ->
                    Layout.none

                Menu menuModel ->
                    Menu.view MenuSpecific defaultOptions menuModel

                Game gameModel ->
                    Game.view GameSpecific defaultOptions gameModel

                Tutorial tutorialModel ->
                    Tutorial.view TutorialSpecific defaultOptions tutorialModel
    in
    [ View.stylesheet
    , body
    ]
        |> Html.div [ Html.Attributes.style "height" "100%" ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
