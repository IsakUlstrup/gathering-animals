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

import Engine.StateMachine as State exposing (State(..))


{-| Animal type, just the state for now
-}
type alias Animal =
    { state : State AnimalState
    }



-- STATE


{-| Animal state
-}
type AnimalState
    = Idle
    | Interact
    | Cooldown



-- STATE CONSTRUCTORS


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



-- STATE TRANSITIONS


{-| Transition animal state to interact
-}
setInteract : Animal -> Animal
setInteract animal =
    { animal | state = State.transition interactState animal.state }



-- STATE PREDICATES


{-| Is animal interacting predicate
-}
isInteracting : Animal -> Bool
isInteracting animal =
    State.isState Interact animal.state


{-| Is animal idle predicate
-}
isIdle : Animal -> Bool
isIdle animal =
    State.isState Idle animal.state


{-| Is animal done interacting predicate
-}
isDoneInteracting : Animal -> Bool
isDoneInteracting animal =
    State.isDone Interact animal.state


{-| Is animal cooling predicate
-}
isCooling : Animal -> Bool
isCooling animal =
    State.isState Cooldown animal.state



-- GENERAL


{-| Animal constructor
-}
new : Animal
new =
    Animal cooldownState


{-| Tick state by dt in ms
-}
tick : Int -> Animal -> Animal
tick dt animal =
    { animal | state = State.tick dt animal.state }


{-| If animal is idle and flag is true, set state to interact.

If interact state timer is done return action

-}
interactIf : Bool -> Animal -> ( Animal, Bool )
interactIf shouldInteract animal =
    if shouldInteract && isIdle animal then
        ( setInteract animal, False )

    else if isDoneInteracting animal then
        ( animal, True )

    else
        ( animal, False )
