module Engine.Resource exposing
    ( Resource
    , ResourceState
    , getLoot
    , hit
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

import Engine.Item as Item exposing (Item)
import Random exposing (Generator, Seed)


{-| Resource state, the ints refer to remaining time in each state
-}
type ResourceState
    = Alive
    | Regrowing Int
    | Hit Int
    | Evade Int
    | Exhausted (List Item)


{-| Resource type, just the state for now
-}
type alias Resource =
    { state : ResourceState }


{-| Resource constructor
-}
new : Resource
new =
    Resource Alive


{-| Set state to alive
-}
setAlive : Resource -> Resource
setAlive resource =
    { resource | state = Alive }


{-| Is state alive predicate
-}
isAlive : Resource -> Bool
isAlive resource =
    case resource.state of
        Alive ->
            True

        _ ->
            False


setEvade : Resource -> Resource
setEvade resource =
    { resource | state = Evade 200 }


isEvade : Resource -> Bool
isEvade resource =
    case resource.state of
        Evade _ ->
            True

        _ ->
            False


{-| Set state to hit
-}
setHit : Resource -> Resource
setHit resource =
    { resource | state = Hit 200 }


{-| Is state hit predicate
-}
isHit : Resource -> Bool
isHit resource =
    case resource.state of
        Hit _ ->
            True

        _ ->
            False


{-| Set state to exhausted with some hardcoded items
-}
setExhausted : Resource -> Resource
setExhausted resource =
    { resource | state = Exhausted [ Item.new '🥥', Item.new '🍓' ] }


{-| Is state exhausted predicate
-}
isExhausted : Resource -> Bool
isExhausted resource =
    case resource.state of
        Exhausted _ ->
            True

        _ ->
            False


{-| Get loot, only return items if state is exhausted
-}
getLoot : Resource -> Maybe (List Item)
getLoot resource =
    case resource.state of
        Exhausted items ->
            Just items

        _ ->
            Nothing


{-| Set state to regrowing, only works if state is exhausted
-}
setRegrowing : Resource -> Resource
setRegrowing resource =
    case resource.state of
        Exhausted _ ->
            { resource | state = Regrowing 1000 }

        _ ->
            resource


{-| Is state regrowing predicate
-}
isRegrowing : Resource -> Bool
isRegrowing resource =
    case resource.state of
        Regrowing _ ->
            True

        _ ->
            False


{-| Loot item at index on exhausted resource
-}
lootAtIndex : Int -> Resource -> ( Resource, Maybe Item )
lootAtIndex index resource =
    case ( resource.state, index >= 0 ) of
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
            ( { resource | state = Exhausted newLoot }, item )

        _ ->
            ( resource, Nothing )


{-| Hit chance, hardcoded for now
-}
rollHit : Generator Bool
rollHit =
    Random.weighted
        ( 25, True )
        [ ( 75, False ) ]


{-| Attempt to hit resource
-}
hit : Seed -> Resource -> ( Resource, Seed )
hit seed resource =
    if isAlive resource then
        let
            ( hitRoll, newSeed ) =
                Random.step rollHit seed
        in
        if hitRoll then
            ( setHit resource, newSeed )

        else
            ( setEvade resource, newSeed )

    else
        ( resource, seed )


{-| Tick resource by dt in ms
-}
tick : Int -> Resource -> Resource
tick dt resource =
    case resource.state of
        Regrowing time ->
            if (time - dt) <= 0 then
                resource |> setAlive

            else
                { resource | state = Regrowing (time - dt) }

        Hit time ->
            if (time - dt) <= 0 then
                resource |> setExhausted

            else
                { resource | state = Hit (time - dt) }

        Evade time ->
            if (time - dt) <= 0 then
                resource |> setAlive

            else
                { resource | state = Evade (time - dt) }

        _ ->
            resource
