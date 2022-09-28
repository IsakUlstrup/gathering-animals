module Animal exposing (constructor, state)

import Engine.Animal as Animal
import Engine.Resource as Resource
import Expect
import Fuzz exposing (int)
import Random
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
        [ fuzz int "Tick new animal by random dt, if above cooldown time of 1000 should be idle" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick randomInt
                    |> Animal.tick 0
                    |> Animal.isIdle
                    |> Expect.equal
                        (randomInt >= 1000)
        , fuzz int "Tick new animal by random dt, get action based on alive resource. If random dt is above cooldown time of 1000 should be True" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick randomInt
                    |> Animal.tick 0
                    |> Animal.interact (Resource.new [])
                    |> Tuple.first
                    |> Animal.isInteracting
                    |> Expect.equal
                        (randomInt >= 1000)
        , fuzz int "Tick new animal by random dt, get action based on dead resource. Should be false" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick randomInt
                    |> Animal.tick 0
                    |> Animal.interact (( Resource.new [], Random.initialSeed 0 ) |> Resource.hit |> Tuple.first)
                    |> Tuple.second
                    |> Expect.equal
                        False
        , fuzz int "Tick new animal by random dt, get action based on alive resource. state should be interact if randomInt is above cooldown time of 1000" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick randomInt
                    |> Animal.tick 0
                    |> Animal.interact (Resource.new [])
                    |> Tuple.first
                    |> Animal.isInteracting
                    |> Expect.equal
                        (randomInt >= 1000)
        , fuzz int "Tick ineracting animal by random dt, get hit, should be true if randomInt is above interact time of 200" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick 2000
                    |> Animal.tick 0
                    |> Animal.interact (Resource.new [])
                    |> Tuple.first
                    |> Animal.tick randomInt
                    |> Animal.interact (Resource.new [])
                    |> Tuple.second
                    |> Expect.equal
                        (randomInt >= 200)
        , fuzz int "Tick interacting animal by random dt, state should be cooling down if randomInt is above interact time of 200" <|
            \randomInt ->
                Animal.new
                    |> Animal.tick 1000
                    |> Animal.tick 0
                    |> Animal.interact (Resource.new [])
                    |> Tuple.first
                    |> Animal.tick randomInt
                    |> Animal.tick 0
                    |> Animal.isCooling
                    |> Expect.equal
                        (randomInt >= 200)
        ]
