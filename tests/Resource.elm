module Resource exposing (constructor, loot, state)

import Content.Items
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
                Resource.new 'a' 'r' []
                    |> Resource.isAlive
                    |> Expect.equal True
        ]


state : Test
state =
    describe "State stuff"
        [ test "Hit alive resource with hardcoded seed that will roll hit, state should be hit" <|
            \_ ->
                Resource.new 'a' 'r' []
                    |> Resource.hitIf True (Random.initialSeed 0)
                    |> Tuple.first
                    |> Resource.isHit
                    |> Expect.equal True
        , test "Hit alive resource with hardcoded seed that will roll miss, state should be miss" <|
            \_ ->
                Resource.new 'a' 'r' []
                    |> Resource.hitIf True (Random.initialSeed 1)
                    |> Tuple.first
                    |> Resource.isEvade
                    |> Expect.equal True
        , fuzz int "Hit alive resource, tick by random int. Should be exhausted" <|
            \randomInt ->
                Resource.new 'a' 'r' []
                    |> Resource.hitIf True (Random.initialSeed 0)
                    |> Tuple.first
                    |> Resource.tick randomInt
                    |> Resource.tick 0
                    |> Resource.isExhausted
                    |> Expect.equal (randomInt >= 200)
        , test "Tick exhausted resource, verify that it's regrowing" <|
            \_ ->
                Resource.new 'a' 'r' []
                    |> Resource.hitIf True (Random.initialSeed 0)
                    |> Tuple.first
                    -- Hit state
                    |> Resource.tick 200
                    |> Resource.tick 0
                    -- Exhausted state
                    |> Resource.tick 200
                    |> Resource.tick 0
                    -- regrowing state
                    |> Resource.isRegrowing
                    |> Expect.equal
                        True
        , fuzz int "Tick regrowing resource by random dt, if above grow time of 3000 should be alive" <|
            \randomInt ->
                Resource.new 'a' 'r' []
                    |> Resource.hitIf True (Random.initialSeed 0)
                    |> Tuple.first
                    |> Resource.tick 200
                    |> Resource.tick 0
                    |> Resource.tick 200
                    |> Resource.tick 0
                    |> Resource.tick randomInt
                    |> Resource.tick 0
                    |> Resource.isAlive
                    |> Expect.equal
                        (randomInt >= 3000)
        , test "Verify regrowing icon" <|
            \_ ->
                Resource.new 'a' 'r' []
                    |> Resource.hitIf True (Random.initialSeed 0)
                    |> Tuple.first
                    |> Resource.tick 200
                    |> Resource.tick 0
                    |> Resource.tick 200
                    |> Resource.tick 0
                    |> Resource.icon
                    |> Expect.equal 'r'
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
        [ test "Get loot from hit resource" <|
            \_ ->
                Resource.new 'a' 'r' []
                    |> Resource.hitIf True (Random.initialSeed 0)
                    |> Tuple.first
                    |> Resource.tick 200
                    -- |> Resource.tick 0
                    |> Resource.getLoot (Random.initialSeed 0)
                    |> Tuple.first
                    |> isJust
                    |> Expect.equal True
        , fuzz int "Hit alive resource, tick by random dt, get loot. If randomInt is above hit state time of 200 should return some loot" <|
            \randomInt ->
                Resource.new 'a' 'r' [ ( 100, Content.Items.coconut ) ]
                    |> Resource.hitIf True (Random.initialSeed 0)
                    |> Tuple.first
                    |> Resource.tick randomInt
                    -- |> Resource.tick 0
                    |> Resource.getLoot (Random.initialSeed 0)
                    |> Tuple.first
                    |> Expect.equal
                        (if randomInt >= 200 then
                            Just [ Content.Items.coconut ]

                         else
                            Nothing
                        )
        , test "Get loot from alive resource, should be nothing" <|
            \_ ->
                Resource.new 'a' 'r' []
                    |> Resource.getLoot (Random.initialSeed 0)
                    |> Tuple.first
                    |> isJust
                    |> Expect.equal False
        ]
