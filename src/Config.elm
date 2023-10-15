module Config exposing (defaultOptions, floorCosts, fps, framesPerComet, maxValue, mineVolume, size, spriteSize, version, winAt)

import PixelEngine.Options as Options exposing (Options)


fps : Float
fps =
    2


version : String
version =
    "v1.0"


winAt : Int
winAt =
    4000


floorCosts : Int
floorCosts =
    500


mineVolume : Int
mineVolume =
    100


maxValue : Int
maxValue =
    250


framesPerComet : Int
framesPerComet =
    let
        secondsPerComet : Float
        secondsPerComet =
            5 * 60
    in
    round <| secondsPerComet * fps


size : Int
size =
    16


spriteSize : Float
spriteSize =
    8


defaultOptions : Options msg
defaultOptions =
    Options.default
        |> Options.withMovementSpeed (1 / fps)
        |> Options.withScale 3
