module GameState exposing (GameState, actionUpdate, lootUpdate, tickUpdate)

import Engine.Animal exposing (Animal)
import Engine.Inventory exposing (Inventory)
import Engine.Item exposing (Item)
import Engine.Resource exposing (Resource)
import Random exposing (Seed)


type alias GameState =
    { animal : Animal
    , resource : Resource
    , loot : List Item
    , inventory : Inventory
    , seed : Seed
    }


tickUpdate : Int -> GameState -> GameState
tickUpdate dt state =
    { state
        | animal =
            state.animal
                |> Engine.Animal.tick dt
        , resource =
            state.resource
                |> Engine.Resource.tick dt
    }


actionUpdate : GameState -> GameState
actionUpdate state =
    let
        ( animal, action ) =
            state.animal
                |> Engine.Animal.disableAutoIf (state.resource |> Engine.Resource.isExhausted)
                |> Engine.Animal.interactIf (Engine.Resource.isAlive state.resource && state.animal.auto)

        ( resource, seed ) =
            state.resource
                |> Engine.Resource.hitIf action state.seed
    in
    { state
        | animal = animal
        , resource = resource
        , seed = seed
    }


lootUpdate : GameState -> GameState
lootUpdate state =
    let
        ( loot, seed ) =
            Engine.Resource.getLoot state.seed state.resource
    in
    { state | loot = (loot |> Maybe.withDefault []) ++ state.loot |> List.take 5, seed = seed }
