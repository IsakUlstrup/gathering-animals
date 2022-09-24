module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, h3, main_, p, text)
import Html.Events exposing (onClick)



-- MODEL


type alias Item =
    Char


type AnimalState
    = Idle
    | Interact Int
    | Cooldown Int


type alias Animal =
    { state : AnimalState }


setIdle : Animal -> Animal
setIdle animal =
    { animal | state = Idle }


setInteract : Animal -> Animal
setInteract animal =
    { animal | state = Interact 500 }


tickState : Int -> Animal -> Animal
tickState dt animal =
    case animal.state of
        Idle ->
            animal

        Interact time ->
            { animal | state = Interact <| time - dt }

        Cooldown time ->
            { animal | state = Cooldown <| time - dt }


setCooldown : Animal -> Animal
setCooldown animal =
    { animal | state = Cooldown 1000 }



-- Branch


tick : Int -> Animal -> Resource -> ( Animal, Resource )
tick dt animal resource =
    case ( animal.state, resource.state ) of
        ( Idle, Alive _ ) ->
            ( animal |> setInteract, resource )

        ( Idle, Dead _ ) ->
            ( animal, resource )

        ( Interact time, _ ) ->
            if time <= 0 then
                ( animal |> setCooldown, resource |> hit 10 )

            else
                ( animal |> tickState dt, resource )

        ( Cooldown time, _ ) ->
            if time <= 0 then
                ( animal |> setIdle, resource )

            else
                ( animal |> tickState dt, resource )

        ( _, Regrowing time ) ->
            if time <= 0 then
                ( animal, resource |> setAlive )

            else
                ( animal, { resource | state = Regrowing (time - dt) } )


type ResourceState
    = Alive ( Int, Int )
    | Regrowing Int
    | Dead (List Item)


type alias Resource =
    { state : ResourceState }


setAlive : Resource -> Resource
setAlive resource =
    { resource | state = Alive ( 100, 100 ) }


setRegrowing : Resource -> Resource
setRegrowing resource =
    { resource | state = Regrowing 5000 }


lootAtIndex : Int -> Resource -> ( Resource, Maybe Item )
lootAtIndex index resource =
    case resource.state of
        Alive _ ->
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


hit : Int -> Resource -> Resource
hit power resource =
    case resource.state of
        Alive health ->
            if Tuple.first health - power > 0 then
                { resource | state = Alive <| Tuple.mapFirst (\h -> h - power) health }

            else
                { resource | state = Dead [ '🥭', '🥥' ] }

        Regrowing _ ->
            resource

        Dead _ ->
            resource


type alias Model =
    { animal : Animal
    , resource : Resource
    , inventory : List Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Animal <| Cooldown 1000)
        (Resource <| Alive ( 100, 100 ))
        []
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Int
    | LootItem Int
    | ResetResource


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( animal, resource ) =
                    tick dt model.animal model.resource
            in
            ( { model
                | animal = animal
                , resource = resource
              }
            , Cmd.none
            )

        LootItem index ->
            let
                ( resource, item ) =
                    model.resource |> lootAtIndex index
            in
            case item of
                Just i ->
                    ( { model | resource = resource, inventory = i :: model.inventory }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ResetResource ->
            ( { model | resource = setRegrowing model.resource }, Cmd.none )



-- VIEW


viewAnimal : Animal -> Html msg
viewAnimal animal =
    div []
        [ h3 [] [ text "Animal" ]
        , p [] [ text <| Debug.toString animal.state ]
        ]


viewLoot : Int -> Item -> Html Msg
viewLoot index item =
    button [ onClick <| LootItem index ] [ text <| Debug.toString item ]


viewResource : Resource -> Html Msg
viewResource resource =
    div []
        [ h3 [] [ text "Resource" ]
        , p []
            (case resource.state of
                Alive health ->
                    [ text "Alive", text <| Debug.toString health ]

                Regrowing time ->
                    [ text "Regrowing "
                    , text <| String.fromInt time
                    ]

                Dead items ->
                    [ text "Dead"
                    , div [] (List.indexedMap viewLoot items)
                    , button [ onClick ResetResource ] [ text "Done" ]
                    ]
            )
        ]


view : Model -> Html Msg
view model =
    main_ []
        [ viewAnimal model.animal
        , viewResource model.resource
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (round >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
