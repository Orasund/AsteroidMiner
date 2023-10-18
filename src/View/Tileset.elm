module View.Tileset exposing
    ( background
    , bigMountain
    , comet
    , container
    , font
    , ground
    , itemBackground
    , merger
    , mine
    , mountain
    , oreGround
    , pipe
    , sorter
    , stone
    , tileset
    , valid
    )

import Building exposing (Volume(..))
import Direction exposing (Direction(..))
import Html exposing (Attribute, Html)
import Lib.Pixel
import PixelEngine.Tile as Tile exposing (Tile, Tileset)


font : Tileset
font =
    Tile.tileset
        { source = "Expire8x8.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }


tileset : Tileset
tileset =
    Tile.tileset
        { source = "tileset.png"
        , spriteWidth = 8
        , spriteHeight = 8
        }


fromPosition : ( Int, Int ) -> List (Attribute msg) -> Html msg
fromPosition pos attrs =
    Lib.Pixel.spriteImage attrs
        { url = "tileset.png"
        , width = 8 * 3
        , height = 8 * 3
        , pos = pos
        , sheetColumns = 17
        , sheetRows = 17
        }


background : Tile msg
background =
    Tile.fromPosition ( 0, 0 )


valid : Tile msg
valid =
    Tile.fromPosition ( 4, 5 )


ground : Tile msg
ground =
    Tile.fromPosition ( 1, 0 )


mountain : Tile msg
mountain =
    Tile.fromPosition ( 0, 1 )


bigMountain : Tile msg
bigMountain =
    Tile.fromPosition ( 1, 1 )


oreGround : Tile msg
oreGround =
    Tile.fromPosition ( 1, 1 )


comet : List (Attribute msg) -> Html msg
comet =
    {--Tile.fromPosition ( 4, 4 )
        |> Tile.animated 4
        |> Tile.movable "comet"--}
    ( 4, 4 ) |> fromPosition


mine : Tile msg
mine =
    Tile.fromPosition ( 4, 2 )
        |> Tile.animated 4


pipe : Tile msg
pipe =
    Tile.fromPosition ( 4, 6 )


container : Volume -> Tile msg
container volume =
    case volume of
        Empty ->
            Tile.fromPosition ( 2, 0 )

        HalfEmpty ->
            Tile.fromPosition ( 3, 0 )

        HalfFull ->
            Tile.fromPosition ( 2, 1 )

        Full ->
            Tile.fromPosition ( 3, 1 )


merger : Tile msg
merger =
    Tile.fromPosition ( 4, 0 )
        |> Tile.animated 4


sorter : Tile msg
sorter =
    Tile.fromPosition ( 4, 1 )
        |> Tile.animated 4


itemBackground : Tile msg
itemBackground =
    Tile.fromPosition ( 2, 2 )


stone : Tile msg
stone =
    Tile.fromPosition ( 3, 2 )
