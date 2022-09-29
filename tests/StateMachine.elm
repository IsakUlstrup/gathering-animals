module StateMachine exposing (get, timed, transition)

import Content.Items
import Engine.Item exposing (Item)
import Engine.StateMachine as State exposing (State(..))
import Expect
import Fuzz exposing (int)
import Test exposing (Test, describe, test)


type TestState
    = Alive
    | Hit
    | Miss
    | Dead (List Item)


loot : List Item
loot =
    [ Content.Items.coconut, Content.Items.mango ]


{-| Alive state, can transition to hit or miss
-}
aliveState : State TestState
aliveState =
    Idle Alive [ Hit, Miss ]


{-| Hit timed state, transition to dead state at the end of timer
-}
hitState : State TestState
hitState =
    Timer 300 Hit deadState


{-| Miss timed state, transition to alive at the end of timer
-}
missState : State TestState
missState =
    Timer 300 Miss aliveState


{-| Dead state, can transition to alive
-}
deadState : State TestState
deadState =
    Idle (Dead loot) [ Alive ]


transition : Test
transition =
    describe "State test"
        [ test "Transition from alive to hit, should be valid" <|
            \_ ->
                aliveState
                    |> State.transition hitState
                    |> Expect.equal
                        (Timer 300 Hit deadState)
        , test "Transition from alive to miss, should be valid" <|
            \_ ->
                aliveState
                    |> State.transition missState
                    |> Expect.equal
                        (Timer 300 Miss aliveState)
        , test "Transition from timer to miss, should not change" <|
            \_ ->
                aliveState
                    |> State.transition missState
                    |> State.transition aliveState
                    |> Expect.equal
                        (Timer 300 Miss aliveState)
        , test "Transition from alive to dead, which is invalid. Should stay alive" <|
            \_ ->
                aliveState
                    |> State.transition deadState
                    |> Expect.equal
                        (Idle Alive [ Hit, Miss ])
        ]


timed : Test
timed =
    describe "Timed state stests"
        [ test "Tick timed state by number above total time, state should be the same with timer at 0" <|
            \_ ->
                aliveState
                    |> State.transition hitState
                    |> State.tick 500
                    |> Expect.equal
                        (Timer 0 Hit deadState)
        , test "Tick timed state by number above total time, then tick again with 0 time to transition to next state, should be dead" <|
            \_ ->
                aliveState
                    |> State.transition hitState
                    |> State.tick 500
                    |> State.tick 0
                    |> Expect.equal
                        (Idle (Dead loot) [ Alive ])
        ]


get : Test
get =
    describe "State getters"
        [ test "State.getState should return current state" <|
            \_ ->
                aliveState
                    |> State.transition hitState
                    |> State.tick 500
                    |> State.tick 0
                    |> State.getState
                    |> Expect.equal
                        (Dead [ Content.Items.coconut, Content.Items.mango ])
        , test "Check if timed state is done (timer == 0), should be true" <|
            \_ ->
                aliveState
                    |> State.transition hitState
                    |> State.tick 500
                    |> State.isDone Hit
                    |> Expect.equal
                        True
        , Test.fuzz int "Check if state timer is done with random tick" <|
            \randomInt ->
                aliveState
                    |> State.transition hitState
                    |> State.tick randomInt
                    |> State.isDone Hit
                    |> Expect.equal
                        (randomInt >= 300)
        , test "Check if state timer is done, but provide wrong type. Should be false" <|
            \_ ->
                aliveState
                    |> State.transition hitState
                    |> State.tick 500
                    |> State.isDone Miss
                    |> Expect.equal
                        False
        ]
