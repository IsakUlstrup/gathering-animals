module Engine.Animal exposing
    ( Animal
    , AnimalState(..)
    , interact
    , isCooling
    , isDoneCooling
    , isIdle
    , new
    , tick
    )

import Engine.Resource as Resource exposing (Resource)


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


isIdle : Animal -> Bool
isIdle animal =
    case animal.state of
        Idle ->
            True

        _ ->
            False


setInteract : Animal -> Animal
setInteract animal =
    { animal | state = interactState }


setCooldown : Animal -> Animal
setCooldown animal =
    { animal | state = cooldownState }


isCooling : Animal -> Bool
isCooling animal =
    case animal.state of
        Cooldown _ ->
            True

        _ ->
            False


isDoneCooling : Animal -> Bool
isDoneCooling animal =
    case animal.state of
        Cooldown 0 ->
            True

        _ ->
            False


tickState : Int -> Animal -> Animal
tickState dt animal =
    case animal.state of
        Idle ->
            animal

        Interact time ->
            { animal | state = Interact <| time - dt }

        Cooldown time ->
            { animal | state = Cooldown <| time - dt }


{-| Advance to next state if timer is done, otherwise reduce current state timer
-}
tickHelper : Int -> Int -> Animal -> (Animal -> Animal) -> Animal
tickHelper time dt animal nextState =
    if (time - dt) <= 0 then
        animal |> nextState

    else
        animal |> tickState dt



-- updateIf : Bool -> (Animal -> Animal) -> Animal -> Animal
-- updateIf pred f animal =
--     if pred then
--         f animal
--     else
--         animal


{-| Tick state by dt in ms
-}
tick : Int -> Animal -> Animal
tick dt animal =
    case animal.state of
        Idle ->
            animal

        --  updateIf (Engine.Resource.isAlive resource) setInteract animal
        Interact time ->
            -- { animal | state = Interact <| max 0 (time - dt) }
            tickHelper time dt animal setCooldown

        -- if time <= 0 then
        --      animal |> setCooldown
        -- else
        --      animal |> tickState dt
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



-- { animal | state = Cooldown <| max 0 (time - dt) }
-- tick : Int -> Resource -> Animal -> ( Animal, Bool )
-- tick dt resource animal =
--     case animal.state of
--         Idle ->
--             ( updateIf (Engine.Resource.isAlive resource) setInteract animal, False )
--         Interact time ->
--             if time <= 0 then
--                 ( animal |> setCooldown, True )
--             else
--                 ( animal |> tickState dt, False )
--         Cooldown time ->
--             ( tickHelper time dt animal setIdle, False )
