module Lib.Command exposing (SingleCommand(..), apply)

import Building exposing (BuildingType)
import Direction exposing (Direction)


type SingleCommand
    = Store
    | Send Direction
    | Create
    | Transition BuildingType
    | Destroy


apply :
    { store : BuildingType -> Result x BuildingType
    , send : Direction -> BuildingType -> Result x BuildingType
    , transition : BuildingType -> BuildingType -> Result x BuildingType
    , create : BuildingType -> Result x BuildingType
    , destroy : BuildingType -> Result x BuildingType
    }
    -> List SingleCommand
    -> BuildingType
    -> BuildingType
apply fun command b =
    command
        |> List.filterMap
            (\c ->
                b
                    |> (case c of
                            Store ->
                                fun.store

                            Send direction ->
                                fun.send direction

                            Transition sort ->
                                fun.transition sort

                            Create ->
                                fun.create

                            Destroy ->
                                fun.destroy
                       )
                    |> Result.toMaybe
            )
        |> List.head
        |> Maybe.withDefault b
