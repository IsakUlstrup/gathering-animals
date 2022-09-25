module Engine.Resource exposing (Resource, ResourceState(..), lootAtIndex, setRegrowing, tick)

import Engine.Item exposing (Item)


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
            in
            ( { resource | state = Dead (first ++ second) }, item )



-- hit : Resource -> Resource
-- hit resource =
--     case resource.state of
--         Alive ->
--             { resource | state = Dead [ '🥭', '🥥' ] }
--         Regrowing _ ->
--             resource
--         Dead _ ->
--             resource
