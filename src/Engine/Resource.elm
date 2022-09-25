module Engine.Resource exposing
    ( Resource
    , ResourceState(..)
    , hit
    , isAlive
    , lootAtIndex
    , setRegrowing
    , tick
    )

import Engine.Item exposing (Item)
import Random exposing (Generator, Seed)


tick : Int -> Resource -> Resource
tick dt resource =
    case resource.state of
        Regrowing time ->
            if time <= 0 then
                resource |> setAlive

            else
                { resource | state = Regrowing (time - dt) }

        _ ->
            resource


type ResourceState
    = Alive
    | Regrowing Int
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


setDead : Resource -> Resource
setDead resource =
    { resource | state = Dead [ '🍓', '🥥' ] }


setRegrowing : Resource -> Resource
setRegrowing resource =
    { resource | state = Regrowing 5000 }


lootAtIndex : Int -> Resource -> ( Resource, Maybe Item )
lootAtIndex index resource =
    case resource.state of
        Alive ->
            ( resource, Nothing )

        Regrowing _ ->
            ( resource, Nothing )

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
            if List.isEmpty newLoot then
                ( setRegrowing resource, item )

            else
                ( { resource | state = Dead newLoot }, item )


rollHit : Generator Bool
rollHit =
    Random.weighted
        ( 20, True )
        [ ( 80, False ) ]


hit : Maybe () -> Seed -> Resource -> ( Resource, Seed )
hit mhit seed resource =
    if isAlive resource then
        case mhit of
            Just _ ->
                let
                    ( isHit, newSeed ) =
                        Random.step rollHit seed
                in
                if isHit then
                    ( setDead resource, newSeed )

                else
                    ( resource, newSeed )

            Nothing ->
                ( resource, seed )

    else
        ( resource, seed )
