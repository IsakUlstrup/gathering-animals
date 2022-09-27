module Animal exposing (constructor, state)

import Engine.Animal as Animal
import Engine.Resource exposing (Resource, ResourceState(..))
import Expect
import Fuzz exposing (int)
import Test exposing (Test, describe, fuzz, test)


constructor : Test
constructor =
    describe "Animal Constructor"
        [ test "Create new animal, should be cooling down" <|
            \_ ->
                Animal.new
                    |> Animal.isCooling
                    |> Expect.equal True
        ]


state : Test
state =
    describe "Animal state"
        [ fuzz int "Tick new animal by random dt, if above 1000 should be done cooling" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick randomInt
                    |> Animal.isIdle
                    |> Expect.equal
                        (randomInt >= 1000)
        , fuzz int "Tick new animal by random dt, get action based on alive resource. If random dt is above 1000 should be True" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick randomInt
                    |> Animal.interact (Resource Alive)
                    |> Tuple.second
                    |> Expect.equal
                        (randomInt >= 1000)
        , fuzz int "Tick new animal by random dt, get action based on dead resource. Should be false" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick randomInt
                    |> Animal.interact (Resource <| Dead [])
                    |> Tuple.second
                    |> Expect.equal
                        False
        ]
