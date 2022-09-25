module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Animal exposing (Animal, AnimalState(..))
import Engine.Item exposing (Item)
import Engine.Resource exposing (Resource, ResourceState(..))
import Html exposing (Html, button, div, h3, main_, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Random exposing (Seed)



-- MODEL


type alias Model =
    { animal : Animal
    , resource : Resource
    , inventory : List Item
    , seed : Seed
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Animal <| Cooldown 1000)
        (Resource <| Alive)
        []
        (Random.initialSeed 130)
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
                ( animal, hit ) =
                    Engine.Animal.tick dt (Engine.Resource.isAlive model.resource) model.animal

                ( resource, seed ) =
                    Engine.Resource.hit hit model.seed model.resource
            in
            ( { model
                | animal = animal
                , resource = resource |> Engine.Resource.tick dt
                , seed = seed
              }
            , Cmd.none
            )

        LootItem index ->
            let
                ( resource, item ) =
                    model.resource |> Engine.Resource.lootAtIndex index
            in
            case item of
                Just i ->
                    ( { model | resource = resource, inventory = i :: model.inventory }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ResetResource ->
            ( { model | resource = Engine.Resource.setRegrowing model.resource }, Cmd.none )



-- VIEW


viewInventory : List Item -> Html msg
viewInventory items =
    let
        viewItem : Item -> Html msg
        viewItem i =
            div [ class "item" ] [ text <| String.fromChar i ]
    in
    div [ class "inventory" ]
        [ h3 [] [ text "Inventory" ]
        , div [ class "items" ] (List.map viewItem items)
        ]


viewAnimal : Animal -> Html msg
viewAnimal animal =
    let
        state =
            case animal.state of
                Idle ->
                    "idle"

                Interact _ ->
                    "interact"

                Cooldown _ ->
                    "cooldown"
    in
    div [ class "animal", class state ]
        [ h3 [ class "name" ] [ text "Animal" ]

        -- , p [] [ text <| Debug.toString animal.state ]
        ]


viewLoot : Int -> Item -> Html Msg
viewLoot index item =
    button [ onClick <| LootItem index ] [ text <| String.fromChar item ]


viewResource : Resource -> Html Msg
viewResource resource =
    div [ class "resource" ]
        [ h3 [] [ text "Resource" ]
        , p []
            (case resource.state of
                Alive ->
                    [ p [] [ text "alive" ]
                    ]

                Regrowing time ->
                    [ text "Regrowing "
                    , text <| String.fromInt time
                    ]

                Hit _ ->
                    [ p [ class "hit" ] [ text "hit" ]
                    ]

                Dead items ->
                    [ text "Dead, loot"
                    , div [] (List.indexedMap viewLoot items)
                    , button [ onClick ResetResource ] [ text "Done" ]
                    ]
            )
        ]


view : Model -> Html Msg
view model =
    main_ [ id "app" ]
        [ div [ class "location" ] [ div [ class "resource" ] [ h3 [] [ text "Resource" ] ] ]
        , div [ class "location" ] [ div [ class "resource" ] [ h3 [] [ text "Resource" ] ] ]
        , div [ class "location" ] [ div [ class "resource" ] [ h3 [] [ text "Resource" ] ] ]
        , div [ class "location" ] [ div [ class "resource" ] [ h3 [] [ text "Resource" ] ] ]
        , div [ class "location" ]
            [ viewAnimal model.animal
            , viewResource model.resource
            ]
        , div [ class "location" ] [ viewInventory model.inventory ]
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
