module Engine.StateMachine exposing
    ( State(..)
    , getState
    , isDone
    , tick
    , transition
    )

{-| State state (list of possible transitions)

Timer time state (state to transition to when timer is done)

-}


type State s
    = State s (List s)
    | TimedState Int s (State s)


{-| Tick state by dt is ms, does nothing if state is State
-}
tick : Int -> State s -> State s
tick dt state =
    case state of
        State _ _ ->
            state

        TimedState time from to ->
            if time <= 0 then
                to

            else
                TimedState (time - dt |> max 0) from to


{-| transition state based on constraints
-}
transition : State s -> State s -> State s
transition newState state =
    case state of
        State _ allowedStates ->
            case newState of
                State ns _ ->
                    if List.member ns allowedStates then
                        newState

                    else
                        state

                TimedState _ from _ ->
                    if List.member from allowedStates then
                        newState

                    else
                        state

        TimedState _ _ _ ->
            state


{-| get current state
-}
getState : State s -> s
getState state =
    case state of
        State s _ ->
            s

        TimedState _ s _ ->
            s


{-| Does state match the given one and is timer done?
-}
isDone : s -> State s -> Bool
isDone s state =
    case state of
        TimedState 0 currState _ ->
            s == currState

        _ ->
            False
