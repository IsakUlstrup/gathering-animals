module Engine.Resource exposing
    ( DropTable
    , Resource
    , ResourceState
    , getLoot
    , hitIf
    , isAlive
    , isEvade
    , isExhausted
    , isHit
    , isRegrowing
    , new
    , tick
    )

import Engine.Item exposing (Item)
import Engine.StateMachine as State exposing (State(..))
import Random exposing (Generator, Seed)


{-| Resource state, the ints refer to remaining time in each state
-}
type ResourceState
    = Alive
    | Hit
    | Evade
    | Exhausted
    | Regrowing


aliveState : State ResourceState
aliveState =
    State Alive [ Hit, Evade ]


hitState : State ResourceState
hitState =
    TimedState 200 Hit exhaustedState


evadeState : State ResourceState
evadeState =
    TimedState 200 Evade aliveState


exhaustedState : State ResourceState
exhaustedState =
    TimedState 200 Exhausted regrowingState


regrowingState : State ResourceState
regrowingState =
    TimedState 1000 Regrowing aliveState


type alias DropTable =
    List ( Float, Item )


{-| Resource type, just the state for now
-}
type alias Resource =
    { state : State ResourceState
    , dropTable : DropTable
    }


{-| Resource constructor
-}
new : DropTable -> Resource
new dropTable =
    Resource aliveState dropTable


{-| Is state alive predicate
-}
isAlive : Resource -> Bool
isAlive resource =
    case State.getState resource.state of
        Alive ->
            True

        _ ->
            False


{-| transition to evade state
-}
setEvade : Resource -> Resource
setEvade resource =
    { resource | state = State.transition evadeState resource.state }


{-| Is state evade predicate
-}
isEvade : Resource -> Bool
isEvade resource =
    case State.getState resource.state of
        Evade ->
            True

        _ ->
            False


{-| Is state hit predicate
-}
isHit : Resource -> Bool
isHit resource =
    case State.getState resource.state of
        Hit ->
            True

        _ ->
            False


{-| Given some loot, transition state to hit. Will transition into exhausted at timer end
-}
setHit : Resource -> Resource
setHit resource =
    { resource | state = State.transition hitState resource.state }


{-| Is state exhausted predicate
-}
isExhausted : Resource -> Bool
isExhausted resource =
    case State.getState resource.state of
        Exhausted ->
            True

        _ ->
            False


{-| Get loot, only return items if state is exhausted
-}
getLoot : Seed -> Resource -> ( Maybe (List Item), Seed )
getLoot seed resource =
    let
        ( loot, newSeed ) =
            Random.step (rollLoot resource.dropTable) seed
    in
    if State.isDone Hit resource.state then
        ( Just loot, newSeed )

    else
        ( Nothing, seed )


{-| Is state regrowing predicate
-}
isRegrowing : Resource -> Bool
isRegrowing resource =
    case State.getState resource.state of
        Regrowing ->
            True

        _ ->
            False


{-| Hit chance, hardcoded for now
-}
rollHit : Generator Bool
rollHit =
    Random.weighted
        ( 25, True )
        [ ( 75, False ) ]


{-| Roll hit chance, set state based on result
-}
resolveHit : Seed -> Resource -> ( Resource, Seed )
resolveHit seed resource =
    let
        ( hitRoll, newSeed ) =
            Random.step rollHit seed
    in
    if hitRoll then
        ( setHit resource, newSeed )

    else
        ( setEvade resource, newSeed )


{-| Attempt to hit resource
-}
hitIf : Bool -> Seed -> Resource -> ( Resource, Seed )
hitIf shouldHit seed resource =
    if isAlive resource && shouldHit then
        resolveHit seed resource

    else
        ( resource, seed )


{-| Generates an item based on drop table, an empty table will allways return Nothing
-}
rollItem : DropTable -> Generator (Maybe Item)
rollItem dropTable =
    Random.weighted
        ( 0, Nothing )
        (List.map (Tuple.mapSecond Just) dropTable)


{-| Generate a list of items based on drop table
-}
rollLoot : DropTable -> Generator (List Item)
rollLoot dropTable =
    Random.weighted ( 40, 1 ) [ ( 30, 2 ), ( 20, 3 ), ( 9, 4 ), ( 1, 5 ) ]
        |> Random.andThen
            (\quantity -> Random.list quantity (rollItem dropTable) |> Random.map (List.filterMap identity))


{-| Tick resource by dt in ms
-}
tick : Int -> Resource -> Resource
tick dt resource =
    { resource | state = State.tick dt resource.state }
