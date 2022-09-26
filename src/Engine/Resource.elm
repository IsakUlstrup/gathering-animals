module Engine.Resource exposing
    ( Resource
    , ResourceState(..)
    , hit
    , isAlive
    , lootAtIndex
    , setRegrowing
    , tick
    )

import Engine.Item exposing (Item(..))
import Random exposing (Generator, Seed)


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
                resource |> setDead

            else
                { resource | state = Hit (time - dt) }

        _ ->
            resource


type ResourceState
    = Alive
    | Regrowing Int
    | Hit Int
    | Dead (List Item)


type alias Resource =
    { state : ResourceState }


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


setDead : Resource -> Resource
setDead resource =
    { resource | state = Dead [ Strawberry, Coconut ] }


setRegrowing : Resource -> Resource
setRegrowing resource =
    { resource | state = Regrowing 1000 }


lootAtIndex : Int -> Resource -> ( Resource, Maybe Item )
lootAtIndex index resource =
    case resource.state of
        Dead loot ->
            let
                item =
                    loot |> List.drop index |> List.head

                first =
                    loot |> List.take index

                second =
                    loot |> List.drop (index + 1)

                newLoot =
                    first ++ second
            in
            ( { resource | state = Dead newLoot }, item )

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
            ( isHit, newSeed ) =
                Random.step rollHit seed
        in
        if isHit then
            ( setHit resource, newSeed )

        else
            ( resource, newSeed )

    else
        ( resource, seed )
