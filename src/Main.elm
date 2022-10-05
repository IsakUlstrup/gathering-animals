module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Resources
import Css exposing (rgb)
import Engine.Animal as Animal exposing (Animal)
import Engine.Inventory exposing (Inventory)
import Engine.Resource as Resource exposing (Resource)
import Html.Styled exposing (Html, div, main_, toUnstyled)
import Html.Styled.Attributes exposing (css, id)
import Json.Decode as Decode exposing (Decoder, Value)
import Random exposing (Seed)
import Storage
import View.MainView
import View.Paper



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
        [ id "app"
        , css
            [ Css.minHeight <| Css.vh 100
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.stretch
            , Css.justifyContent Css.center
            , View.Paper.paperSolid <| rgb 255 255 200
            , Css.fontFamily Css.sansSerif
            ]
        ]
        [ div
            [ css
                [ View.MainView.locationStyle
                , View.Paper.paperGradient [ rgb 250 50 0, rgb 255 50 100 ]
                ]
            ]
            [ View.MainView.viewAnimal model.animal
            , View.MainView.viewResource LootItem ResetResource model.resource
            ]
        , div
            [ css
                [ View.MainView.locationStyle
                , View.Paper.paperGradient [ rgb 130 50 70, rgb 30 50 100 ]
                , View.MainView.flexCenter
                ]
            ]
            [ View.MainView.viewInventory model.inventory ]
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
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
