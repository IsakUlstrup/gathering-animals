module Engine.Animal exposing (Animal, AnimalState(..), tick)


type AnimalState
    = Idle
    | Interact Int
    | Cooldown Int


type alias Animal =
    { state : AnimalState }


setIdle : Animal -> Animal
setIdle animal =
    { animal | state = Idle }


setInteract : Animal -> Animal
setInteract animal =
    { animal | state = Interact 200 }


tickState : Int -> Animal -> Animal
tickState dt animal =
    case animal.state of
        Idle ->
            animal

        Interact time ->
            { animal | state = Interact <| time - dt }

        Cooldown time ->
            { animal | state = Cooldown <| time - dt }


setCooldown : Animal -> Animal
setCooldown animal =
    { animal | state = Cooldown 1000 }


tick : Int -> Animal -> Animal
tick dt animal =
    case animal.state of
        Idle ->
            animal |> setInteract

        Interact time ->
            if time <= 0 then
                animal |> setCooldown

            else
                animal |> tickState dt

        Cooldown time ->
            if time <= 0 then
                animal |> setIdle

            else
                animal |> tickState dt
