module Main exposing (Model, Msg, main)

-- import Html.Attributes exposing (class, id)
-- import Html.Events exposing (onClick)

import Browser
import Browser.Events
import Css exposing (px)
import Css.Transitions as Transitions
import Engine.Animal as Animal exposing (Animal, AnimalState(..))
import Engine.Item exposing (Item)
import Engine.Resource as Resource exposing (Resource, ResourceState(..))
import Html.Styled exposing (Attribute, Html, button, div, h3, main_, p, text, toUnstyled)
import Html.Styled.Attributes as Html exposing (class, css, id)
import Html.Styled.Events
import Json.Decode as Decode exposing (Value)
import Random exposing (Seed)
import Storage



-- MODEL


type alias Model =
    { animal : Animal
    , resource : Resource
    , inventory : List Item
    , seed : Seed
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        inventory : Result String (List Item)
        inventory =
            case Decode.decodeValue Decode.string flags of
                Ok invJson ->
                    Storage.decodeStoredInventory invJson

                Err _ ->
                    Err "Invalid data from port"
    in
    case inventory of
        Ok inv ->
            ( Model
                Animal.new
                (Resource <| Alive)
                inv
                (Random.initialSeed 130)
            , Cmd.none
            )

        Err _ ->
            ( Model
                Animal.new
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
                    Animal.tick dt model.resource model.animal

                ( resource, seed ) =
                    if hit then
                        Resource.tick dt model.resource
                            |> Resource.hit model.seed

                    else
                        ( Resource.tick dt model.resource, model.seed )
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
                        , inventory = item :: model.inventory
                    }
                        |> (\m -> ( m, Storage.saveInventory m.inventory ))

                ( _, Nothing ) ->
                    ( model, Cmd.none )

        ResetResource ->
            ( { model | resource = Resource.setRegrowing model.resource }, Cmd.none )



-- VIEW


viewInventory : List Item -> Html msg
viewInventory items =
    let
        viewItem : Item -> Html msg
        viewItem i =
            div [ class "item" ] [ text <| String.fromChar <| Engine.Item.toEmoji i ]
    in
    div [ class "inventory" ]
        [ h3 [] [ text "Inventory" ]
        , div [ class "items", css [ Css.displayFlex ] ] (List.map viewItem items)
        ]


viewAnimal : Animal -> Html msg
viewAnimal animal =
    let
        state : List Css.Style
        state =
            case animal.state of
                Interact _ ->
                    [ Css.transform <| Css.translate2 (px 100) (px 0) ]

                _ ->
                    []
    in
    div
        [ css
            [ Css.flex <| Css.int 1
            , Css.border2 (Css.px 1) Css.solid
            , Transitions.transition [ Transitions.transform 1000 ]
            ]
        ]
        [ h3 [ css (Transitions.transition [ Transitions.transform 1000 ] :: state) ] [ text "Animal" ] ]


viewLoot : Int -> Item -> Html Msg
viewLoot index item =
    button [ Html.Styled.Events.onClick <| LootItem index ]
        [ text <| String.fromChar <| Engine.Item.toEmoji item ]


viewResource : Resource -> Html Msg
viewResource resource =
    div [ class "resource", css [ Css.flex <| Css.int 1, Css.border2 (Css.px 1) Css.dotted ] ]
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
                    , button [ Html.Styled.Events.onClick ResetResource ] [ text "Done" ]
                    ]
            )
        ]



-- position: relative
--     font-family: Arial, Helvetica, sans-serif
--     min-height: 100vh
--     display: flex
--     flex-direction: column
--     align-items: stretch
--     justify-content: center
-- display: flex
--     flex: 1 1 0
--     min-height: 15rem


locationStyle : Attribute msg
locationStyle =
    css
        [ Css.displayFlex
        , Css.flex3 (Css.int 1) (Css.int 1) (Css.int 0)
        , Css.minHeight (Css.rem 15)
        ]


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
            ]
        ]
        [ div [ class "location" ]
            [ div [ class "resource" ] [ h3 [] [ text "Resource" ] ] ]
        , div
            [ Html.css
                [ Css.displayFlex ]
            ]
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


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
