module Data.ToolSelection exposing (..)

import Data.Item exposing (Item)


type ToolSelection
    = Mine
    | Pipe
    | Container
    | Merger
    | Sorter
    | Floor
    | Delete
    | Bag (Maybe Item)
