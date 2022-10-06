module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Resources
import Engine.Animal as Animal exposing (Animal)
import Engine.Inventory exposing (Inventory)
import Engine.Resource as Resource exposing (Resource)
import Html exposing (Html, main_)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, Value)
import Random exposing (Seed)
import Storage
import View



-- MODEL


type alias Model =
    { animal : Animal
    , resource : Resource
    , inventory : Inventory
    , seed : Seed
    }


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
            ( Model
                Animal.new
                Content.Resources.test
                cfg.saveData
                (Random.initialSeed cfg.time)
            , Cmd.none
            )

        Err _ ->
            ( Model
                Animal.new
                Content.Resources.test
                []
                (Random.initialSeed 134857)
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
                ( animal, action ) =
                    model.animal
                        |> Animal.tick dt
                        |> Animal.interactIf (Resource.isAlive model.resource)

                ( resource, seed ) =
                    model.resource
                        |> Resource.tick dt
                        |> Resource.hitIf action model.seed
            in
            ( { model
                | animal = animal
                , resource = resource
                , seed = seed
              }
            , Cmd.none
            )

        LootItem index ->
            case model.resource |> Resource.lootAtIndex index of
                ( newResource, Just item ) ->
                    { model
                        | resource = newResource
                        , inventory = Engine.Inventory.add item model.inventory
                    }
                        |> (\m -> ( m, Storage.saveInventory m.inventory ))

                ( _, Nothing ) ->
                    ( model, Cmd.none )

        ResetResource ->
            ( { model | resource = Resource.setRegrowing model.resource }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_
        []
        [ View.viewLocation [ class "red-background" ]
            [ View.viewAnimal model.animal
            , View.viewResource LootItem ResetResource model.resource
            ]
        , View.viewLocation [ class "cyan-background" ] [ View.viewInventory model.inventory ]
        ]



-- SUBSCRIPTIONS


disableTick : Model -> Bool
disableTick model =
    Resource.isExhausted model.resource && Animal.isIdle model.animal


subscriptions : Model -> Sub Msg
subscriptions model =
    if disableTick model then
        Sub.none

    else
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
