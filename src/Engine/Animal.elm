module Engine.Animal exposing
    ( Animal
    , AnimalState(..)
    , cooldownState
    , new
    , tick
    )

import Engine.Resource exposing (Resource)


type AnimalState
    = Idle
    | Interact Int
    | Cooldown Int


idleState : AnimalState
idleState =
    Idle


interactState : AnimalState
interactState =
    Interact 200


cooldownState : AnimalState
cooldownState =
    Cooldown 1000


type alias Animal =
    { state : AnimalState
    }


new : Animal
new =
    Animal cooldownState


setIdle : Animal -> Animal
setIdle animal =
    { animal | state = idleState }


setInteract : Animal -> Animal
setInteract animal =
    { animal | state = interactState }


setCooldown : Animal -> Animal
setCooldown animal =
    { animal | state = cooldownState }


tickState : Int -> Animal -> Animal
tickState dt animal =
    case animal.state of
        Idle ->
            animal

        Interact time ->
            { animal | state = Interact <| time - dt }

        Cooldown time ->
            { animal | state = Cooldown <| time - dt }


tickHelper : Int -> Int -> Animal -> (Animal -> Animal) -> Animal
tickHelper time dt animal nextState =
    if time <= 0 then
        animal |> nextState

    else
        animal |> tickState dt


updateIf : Bool -> (Animal -> Animal) -> Animal -> Animal
updateIf pred f animal =
    if pred then
        f animal

    else
        animal


tick : Int -> Resource -> Animal -> ( Animal, Bool )
tick dt resource animal =
    case animal.state of
        Idle ->
            ( updateIf (Engine.Resource.isAlive resource) setInteract animal, False )

        Interact time ->
            if time <= 0 then
                ( animal |> setCooldown, True )

            else
                ( animal |> tickState dt, False )

        Cooldown time ->
            ( tickHelper time dt animal setIdle, False )
