module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Resources
import Engine.Animal as Animal
import Engine.Inventory exposing (Inventory)
import Engine.Item exposing (Item)
import GameState exposing (GameState, actionUpdate, lootUpdate, tickUpdate)
import Html exposing (Html, div, main_)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, Value)
import Random
import Storage
import View



-- MODEL


type alias Model =
    GameState


type alias Config =
    { saveData : Inventory
    , time : Int
    }


configDecoder : Decoder Config
configDecoder =
    Decode.map2 Config
        (Decode.field "saveData" Storage.inventoryDecoder)
        (Decode.field "time" Decode.int)


init : Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue configDecoder flags of
        Ok cfg ->
            ( GameState
                Animal.new
                Content.Resources.evergreen
                []
                cfg.saveData
                (Random.initialSeed cfg.time)
            , Cmd.none
            )

        Err _ ->
            ( GameState
                Animal.new
                Content.Resources.evergreen
                []
                []
                (Random.initialSeed 134857)
            , Cmd.none
            )



-- UPDATE


type Msg
    = Tick Int
    | Interact
    | LootItem Int


lootAtIndex : Int -> List Item -> ( List Item, Maybe Item )
lootAtIndex index items =
    case ( items, index >= 0 ) of
        ( loot, True ) ->
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
            ( newLoot, item )

        _ ->
            ( items, Nothing )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> tickUpdate dt
                |> actionUpdate
                |> lootUpdate
            , Cmd.none
            )

        Interact ->
            ( { model | animal = Animal.toggleAuto model.animal }, Cmd.none )

        LootItem index ->
            case model.loot |> lootAtIndex index of
                ( newLoot, Just item ) ->
                    { model
                        | loot = newLoot
                        , inventory = Engine.Inventory.add item model.inventory
                    }
                        |> (\m -> ( m, Storage.saveInventory m.inventory ))

                ( _, Nothing ) ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_
        []
        [ div [ class "area" ]
            [ Html.h1 [ class "red" ] [ Html.text "Forest" ]
            , View.viewLocation []
                [ div [ class "animal-v-resource" ]
                    [ View.viewAnimal model.animal
                    , View.viewResource Interact model.resource
                    ]
                , View.viewLoot LootItem model.loot
                ]
            ]
        , div [ class "area" ]
            [ Html.h1 [ class "cyan" ] [ Html.text "Debug" ]
            , View.viewLocation [] [ View.viewInventory model.inventory ]
            ]
        , div [ class "area" ]
            [ Html.h1 [ class "cyan" ] [ Html.text "Links" ]
            , View.viewLocation [] [ View.viewSocial ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (round >> Tick)



-- MAIN


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
