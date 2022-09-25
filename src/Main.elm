module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Animal exposing (Animal, AnimalState(..))
import Engine.Item exposing (Item)
import Engine.Resource exposing (Resource, ResourceState(..))
import Html exposing (Html, button, div, h3, main_, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    { animal : Animal
    , resource : Resource
    , inventory : List Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Animal <| Cooldown 1000)
        (Resource <| Alive)
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
            ( { model
                | animal = Engine.Animal.tick dt model.animal
                , resource = Engine.Resource.tick dt model.resource
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

                Dead items ->
                    [ text "Dead, loot"
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
