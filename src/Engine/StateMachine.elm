module Engine.StateMachine exposing (State(..), getState, tick, transition)

{-| Idle state (list of possible transitions)

Timer time state (state to transition to when timer is done)

-}


type State s
    = Idle s (List s)
    | Timer Int s (State s)


{-| Tick state by dt is ms, does nothing if state is idle
-}
tick : Int -> State s -> State s
tick dt state =
    case state of
        Idle _ _ ->
            state

        Timer time from to ->
            if time <= 0 then
                to

            else
                Timer (time - dt |> max 0) from to


{-| transition state based on constraints
-}
transition : State s -> State s -> State s
transition newState state =
    case state of
        Idle _ allowedStates ->
            case newState of
                Idle ns _ ->
                    if List.member ns allowedStates then
                        newState

                    else
                        state

                Timer _ from _ ->
                    if List.member from allowedStates then
                        newState

                    else
                        state

        Timer _ _ _ ->
            state


{-| get current state
-}
getState : State s -> s
getState state =
    case state of
        Idle s _ ->
            s

        Timer _ s _ ->
            s
