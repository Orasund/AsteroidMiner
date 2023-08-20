module Data.ToolSelection exposing (..)

import Data.Item exposing (Item)


type BeltColor
    = Red
    | Blue
    | Green
    | Yellow


type ToolSelection
    = Mine
    | ConveyorBelt BeltColor
    | Pipe
    | Container
    | Merger
    | Sorter
    | Floor
    | Delete
    | Bag (Maybe Item)
