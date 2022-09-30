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
    , lootAtIndex
    , new
    , setRegrowing
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
    | Exhausted (List Item)
    | Regrowing


aliveState : State ResourceState
aliveState =
    State Alive [ Hit, Evade ]


hitState : List Item -> State ResourceState
hitState items =
    TimedState 200 Hit (exhaustedState items)


evadeState : State ResourceState
evadeState =
    TimedState 200 Evade aliveState


exhaustedState : List Item -> State ResourceState
exhaustedState items =
    State (Exhausted items) [ Regrowing ]


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
setHit : List Item -> Resource -> Resource
setHit items resource =
    { resource | state = State.transition (hitState items) resource.state }


{-| Is state exhausted predicate
-}
isExhausted : Resource -> Bool
isExhausted resource =
    case State.getState resource.state of
        Exhausted _ ->
            True

        _ ->
            False


{-| Get loot, only return items if state is exhausted
-}
getLoot : Resource -> Maybe (List Item)
getLoot resource =
    case State.getState resource.state of
        Exhausted items ->
            Just items

        _ ->
            Nothing


{-| Set state to regrowing, only works if state is exhausted
-}
setRegrowing : Resource -> Resource
setRegrowing resource =
    { resource | state = State.transition regrowingState resource.state }


{-| Is state regrowing predicate
-}
isRegrowing : Resource -> Bool
isRegrowing resource =
    case State.getState resource.state of
        Regrowing ->
            True

        _ ->
            False


{-| Loot item at index on exhausted resource
-}
lootAtIndex : Int -> Resource -> ( Resource, Maybe Item )
lootAtIndex index resource =
    case ( State.getState resource.state, index >= 0 ) of
        ( Exhausted loot, True ) ->
            let
                item : Maybe Item
                item =
                    loot |> List.drop index |> List.head

                first : List Item
                first =
                    loot |> List.take index

                second : List Item
                second =
                    loot |> List.drop (index + 1)

                newLoot : List Item
                newLoot =
                    first ++ second
            in
            ( { resource | state = exhaustedState newLoot }, item )

        _ ->
            ( resource, Nothing )


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

        ( loot, newSeed2 ) =
            Random.step (rollLoot resource.dropTable) newSeed
    in
    if hitRoll then
        ( setHit loot resource, newSeed2 )

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
