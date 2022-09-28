module Resource exposing (constructor, loot, state)

import Engine.Resource as Resource
import Expect
import Fuzz exposing (int)
import Random
import Test exposing (Test, describe, fuzz, test)


constructor : Test
constructor =
    describe "Constructors"
        [ test "Create new resource with empty drop table, verify state is alive" <|
            \_ ->
                Resource.new []
                    |> Resource.isAlive
                    |> Expect.equal True
        ]


state : Test
state =
    describe "State stuff"
        [ test "Hit alive resource with hardcoded seed that will roll hit, state should be hit" <|
            \_ ->
                ( Resource.new [], Random.initialSeed 0 )
                    |> Resource.hitIf True
                    |> Tuple.first
                    |> Resource.isHit
                    |> Expect.equal True
        , test "Hit alive resource with hardcoded seed that will roll miss, state should be miss" <|
            \_ ->
                ( Resource.new [], Random.initialSeed 1 )
                    |> Resource.hitIf True
                    |> Tuple.first
                    |> Resource.isEvade
                    |> Expect.equal True
        , fuzz int "Hit alive resource, tick by random dt, get loot. If randomInt is above hit state time of 200 should return some loot" <|
            \randomInt ->
                ( Resource.new [], Random.initialSeed 0 )
                    |> Resource.hitIf True
                    |> Resource.tick randomInt
                    |> Tuple.first
                    |> Resource.isExhausted
                    |> Expect.equal
                        (randomInt >= 200)
        , test "Set alive resource to regrow. should remain in alive state" <|
            \_ ->
                Resource.new []
                    |> Resource.setRegrowing
                    |> Resource.isAlive
                    |> Expect.equal
                        True
        , test "Set exhausted resource to regrow." <|
            \_ ->
                ( Resource.new [], Random.initialSeed 0 )
                    |> Resource.hitIf True
                    |> Resource.tick 200
                    |> Tuple.first
                    |> Resource.setRegrowing
                    |> Resource.isRegrowing
                    |> Expect.equal
                        True
        , fuzz int "Tick regrowing resource by random dt, if above grow time of 1000 should be alive" <|
            \randomInt ->
                ( Resource.new [], Random.initialSeed 0 )
                    |> Resource.hitIf True
                    |> Resource.tick 200
                    |> (\( r, s ) -> ( Resource.setRegrowing r, s ))
                    |> Resource.tick randomInt
                    |> Tuple.first
                    |> Resource.isAlive
                    |> Expect.equal
                        (randomInt >= 1000)
        ]


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        _ ->
            False


loot : Test
loot =
    describe "Loot"
        [ test "Get loot from exhausted resource" <|
            \_ ->
                ( Resource.new [], Random.initialSeed 0 )
                    |> Resource.hitIf True
                    |> Resource.tick 200
                    |> Tuple.first
                    |> Resource.getLoot
                    |> isJust
                    |> Expect.equal True
        , test "Get loot from alive resource, should be nothing" <|
            \_ ->
                Resource.new []
                    |> Resource.getLoot
                    |> isJust
                    |> Expect.equal False
        , fuzz int "Loot at random index in loot list with length of 2, check item" <|
            \randomInt ->
                ( Resource.new [], Random.initialSeed 0 )
                    |> Resource.hitIf True
                    |> Resource.tick 200
                    |> Tuple.first
                    |> Resource.lootAtIndex randomInt
                    |> Tuple.second
                    |> isJust
                    |> Expect.equal
                        (randomInt >= 0 && randomInt <= 1)
        ]
