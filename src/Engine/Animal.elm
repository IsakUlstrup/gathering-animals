module Engine.Animal exposing
    ( Animal
    , AnimalState
    , interactIf
    , isCooling
    , isIdle
    , isInteracting
    , new
    , tick
    )

{-| Animal state, the integers represent remaining time in each state
-}

import Engine.StateMachine as State exposing (State(..))


type AnimalState
    = Idle
    | Interact
    | Cooldown


{-| Animal type, just the state for now
-}
type alias Animal =
    { state : State AnimalState
    }


{-| Idle state constructor
-}
idleState : State AnimalState
idleState =
    State Idle [ Interact ]


{-| Interact state constructor
-}
interactState : State AnimalState
interactState =
    TimedState 200 Interact cooldownState


{-| Cooldown state constructor
-}
cooldownState : State AnimalState
cooldownState =
    TimedState 1000 Cooldown idleState


{-| Animal constructor
-}
new : Animal
new =
    Animal cooldownState


{-| Is animal idle predicate
-}
isIdle : Animal -> Bool
isIdle animal =
    case State.getState animal.state of
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
    case State.getState animal.state of
        Interact ->
            True

        _ ->
            False


isDoneInteracting : Animal -> Bool
isDoneInteracting animal =
    State.isDone Interact animal.state


{-| Is animal cooling predicate
-}
isCooling : Animal -> Bool
isCooling animal =
    case State.getState animal.state of
        Cooldown ->
            True

        _ ->
            False


{-| Tick state by dt in ms
-}
tick : Int -> Animal -> Animal
tick dt animal =
    { animal | state = State.tick dt animal.state }


{-| If resource is alive and animal is idle, set state to interact and return action
-}
interactIf : Bool -> Animal -> ( Animal, Bool )
interactIf shouldInteract animal =
    if shouldInteract && isIdle animal then
        ( setInteract animal, False )

    else if isDoneInteracting animal then
        ( animal, True )

    else
        ( animal, False )
