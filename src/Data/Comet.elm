module Data.Comet exposing (Comet, new, position, update)

import Data exposing (framesPerComet, size)
import Data.Map exposing (Map)
import Grid.Bordered as Grid
import Location exposing (Angle(..))
import Position
import Random exposing (Generator)


type alias Comet =
    { life : Int
    , offset : Angle
    , slope : Float
    }


new : { angle : Angle, moveClockwise : Bool } -> Comet
new args =
    { life = framesPerComet
    , offset = args.angle
    , slope =
        if args.moveClockwise then
            -1

        else
            1
    }


{-| Conchosprial
<https://en.wikipedia.org/wiki/Conchospiral>
-}
asteroidCoord : { t : Int, angle : Angle, slope : Float } -> { x : Int, y : Int }
asteroidCoord args =
    let
        (Angle offset) =
            args.angle

        t : Float
        t =
            toFloat <| args.t

        maximalCycles : Float
        maximalCycles =
            2

        --16
        maximalRadius : Float
        maximalRadius =
            toFloat <| size // 2 - 1

        --Rotation Speed
        a : Float
        a =
            maximalRadius / (toFloat <| framesPerComet)

        --Slope
        c : Float
        c =
            args.slope

        --opening Angle
        --we need angle = logBase mu (c*framesPerComet)= 2*pi*maximalCycles
        --for M:=maximalCycles, T:=framesPerComet
        --we obtained lg(c*T)/2*pi*M = lg(mu)
        --which can be transformed into:
        --  mu = 2^(lg(c*T)/2*pi*M)
        mu : Float
        mu =
            2 ^ (logBase 2 (c * (toFloat <| framesPerComet)) / (2 * pi * maximalCycles))

        radius : Float
        radius =
            a * t

        angle : Float
        angle =
            offset + logBase mu (c * t)

        ( x, y ) =
            fromPolar ( radius, angle )
    in
    { x = round <| x
    , y = round <| y
    }



{- let
       scale : Float
       scale =
           (toFloat <| size // 2 - 1)
               * logBase
                   (toFloat <| framesPerComet)
                   (toFloat <| cyclesUntilComet)

       maximalCycles : Float
       maximalCycles =
           16

       { x, y } =
           Location.fromAngle
               (Angle <|
                   (*) (2 * pi) <|
                       (maximalCycles
                           * (toFloat <| (framesPerComet - cyclesUntilComet) ^ 2)
                       )
                           / (toFloat <| framesPerComet ^ 2)
               )
               |> Location.scaleBy scale
   in
   { x = round <| x
   , y = round <| y
   }
-}


position : Comet -> ( Int, Int )
position comet =
    let
        center : Int
        center =
            size // 2
    in
    ( center, center )
        |> Position.add
            (asteroidCoord
                { t = comet.life
                , angle = comet.offset
                , slope = comet.slope
                }
            )


update : Map -> Comet -> Generator ( Comet, Map )
update map ({ life } as comet) =
    let
        ( x, y ) =
            comet |> position

        defaultCase : Generator ( Comet, Map )
        defaultCase =
            Random.constant
                ( if life > 1 then
                    { comet
                        | life = life - 1
                    }

                  else
                    comet
                , map
                )

        impactCase : Generator ( Comet, Map )
        impactCase =
            map
                |> Grid.remove ( x, y )
                |> Result.map
                    (Grid.ignoringErrors (Grid.remove ( x + 1, y ))
                        >> Grid.ignoringErrors (Grid.remove ( x - 1, y ))
                        >> Grid.ignoringErrors (Grid.remove ( x, y + 1 ))
                        >> Grid.ignoringErrors (Grid.remove ( x, y - 1 ))
                        >> (\m ->
                                Random.map2
                                    (\float bool ->
                                        ( new
                                            { angle = Angle float
                                            , moveClockwise = bool
                                            }
                                        , m
                                        )
                                    )
                                    (Random.float 0 (2 * pi))
                                    (Random.int 0 1 |> Random.map ((==) 0))
                           )
                    )
                |> Result.withDefault defaultCase
    in
    case map |> Grid.get ( x, y ) of
        Ok (Just _) ->
            impactCase

        _ ->
            defaultCase
