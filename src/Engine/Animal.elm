module Engine.Animal exposing
    ( Animal
    , AnimalState
    , interact
    , isCooling
    , isIdle
    , isInteracting
    , new
    , tick
    )

import Engine.Resource as Resource exposing (Resource)


{-| Animal state, the integers represent remaining time in each state
-}
type AnimalState
    = Idle
    | Interact Int
    | Cooldown Int


{-| Idle state constructor
-}
idleState : AnimalState
idleState =
    Idle


{-| Interact state constructor
-}
interactState : AnimalState
interactState =
    Interact 200


{-| Cooldown state constructor
-}
cooldownState : AnimalState
cooldownState =
    Cooldown 1000


{-| Animal type, just the state for now
-}
type alias Animal =
    { state : AnimalState
    }


{-| Animal constructor
-}
new : Animal
new =
    Animal cooldownState


{-| Set animal state to idle
-}
setIdle : Animal -> Animal
setIdle animal =
    { animal | state = idleState }


{-| Is animal idle predicate
-}
isIdle : Animal -> Bool
isIdle animal =
    case animal.state of
        Idle ->
            True

        _ ->
            False


{-| Set animal state to interact
-}
setInteract : Animal -> Animal
setInteract animal =
    { animal | state = interactState }


{-| Is animal interacting predicate
-}
isInteracting : Animal -> Bool
isInteracting animal =
    case animal.state of
        Interact _ ->
            True

        _ ->
            False


{-| Set animal state to cooldown
-}
setCooldown : Animal -> Animal
setCooldown animal =
    { animal | state = cooldownState }


{-| Is animal cooling predicate
-}
isCooling : Animal -> Bool
isCooling animal =
    case animal.state of
        Cooldown _ ->
            True

        _ ->
            False


{-| Reduce remaining time for each state, does not advance state
-}
tickState : Int -> Animal -> Animal
tickState dt animal =
    let
        reduceTime : Int -> Int -> Int
        reduceTime t d =
            t - d |> max 0
    in
    case animal.state of
        Idle ->
            animal

        Interact time ->
            { animal | state = Interact <| reduceTime time dt }

        Cooldown time ->
            { animal | state = Cooldown <| reduceTime time dt }


{-| Advance to next state if timer is done, otherwise reduce current state timer
-}
tickHelper : Int -> Int -> Animal -> (Animal -> Animal) -> Animal
tickHelper time dt animal nextState =
    if (time - dt) <= 0 then
        animal |> nextState

    else
        animal |> tickState dt


{-| Tick state by dt in ms
-}
tick : Int -> Animal -> Animal
tick dt animal =
    case animal.state of
        Idle ->
            animal

        Interact time ->
            tickHelper time dt animal setCooldown

        Cooldown time ->
            tickHelper time dt animal setIdle


{-| If resource is alive and animal is idle, set state to interact and return action
-}
interact : Resource -> Animal -> ( Animal, Bool )
interact resource animal =
    if Resource.isAlive resource && isIdle animal then
        ( setInteract animal, True )

    else
        ( animal, False )
