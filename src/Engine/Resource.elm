module Engine.Resource exposing
    ( Resource
    , ResourceState
    , getLoot
    , hit
    , isAlive
    , isHit
    , isRegrowing
    , lootAtIndex
    , new
    , setRegrowing
    , tick
    )

import Engine.Item exposing (Item(..))
import Random exposing (Generator, Seed)


type ResourceState
    = Alive
    | Regrowing Int
    | Hit Int
    | Exhausted (List Item)


type alias Resource =
    { state : ResourceState }


new : Resource
new =
    Resource Alive


setAlive : Resource -> Resource
setAlive resource =
    { resource | state = Alive }


isAlive : Resource -> Bool
isAlive resource =
    case resource.state of
        Alive ->
            True

        _ ->
            False


setHit : Resource -> Resource
setHit resource =
    { resource | state = Hit 200 }


isHit : Resource -> Bool
isHit resource =
    case resource.state of
        Hit _ ->
            True

        _ ->
            False


setExhausted : Resource -> Resource
setExhausted resource =
    { resource | state = Exhausted [ Strawberry, Coconut ] }


getLoot : Resource -> Maybe (List Item)
getLoot resource =
    case resource.state of
        Exhausted items ->
            Just items

        _ ->
            Nothing


setRegrowing : Resource -> Resource
setRegrowing resource =
    { resource | state = Regrowing 1000 }


isRegrowing : Resource -> Bool
isRegrowing resource =
    case resource.state of
        Regrowing _ ->
            True

        _ ->
            False


lootAtIndex : Int -> Resource -> ( Resource, Maybe Item )
lootAtIndex index resource =
    case resource.state of
        Exhausted loot ->
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


rollHit : Generator Bool
rollHit =
    Random.weighted
        ( 25, True )
        [ ( 75, False ) ]


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
            ( resource, newSeed )

    else
        ( resource, seed )


tick : Int -> Resource -> Resource
tick dt resource =
    case resource.state of
        Regrowing time ->
            if time <= 0 then
                resource |> setAlive

            else
                { resource | state = Regrowing (time - dt) }

        Hit time ->
            if time <= 0 then
                resource |> setExhausted

            else
                { resource | state = Hit (time - dt) }

        _ ->
            resource
