module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Items
import Css exposing (Style, px, rgb)
import Css.Animations
import Css.Transitions as Transitions
import Engine.Animal as Animal exposing (Animal)
import Engine.Item exposing (Item)
import Engine.Resource as Resource exposing (Resource)
import Html.Styled exposing (Attribute, Html, button, div, h3, main_, p, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, id)
import Html.Styled.Events
import Json.Decode as Decode exposing (Value)
import Random exposing (Seed)
import Storage
import View.CssExtra
import View.Paper



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

        resource : Resource
        resource =
            Resource.new
                [ ( 1, Content.Items.mango )
                , ( 9, Content.Items.coconut )
                , ( 90, Content.Items.strawberry )
                ]
    in
    case inventory of
        Ok inv ->
            ( Model
                Animal.new
                resource
                inv
                (Random.initialSeed 134857)
            , Cmd.none
            )

        Err _ ->
            ( Model
                Animal.new
                resource
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
                        |> Animal.interact model.resource

                ( resource, seed ) =
                    ( model.resource, model.seed )
                        |> Resource.tick dt
                        |> Resource.hitIf action
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
            div [ class "item" ] [ text <| Engine.Item.iconString i ]
    in
    div
        [ css
            [ Css.padding <| Css.rem 1
            , Css.displayFlex
            , Css.flexDirection Css.column
            , View.CssExtra.gap 0.5
            ]
        ]
        [ h3 [] [ text "Inventory" ]
        , div
            [ class "items"
            , css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                , View.CssExtra.gap 1
                ]
            ]
            (List.map viewItem items)
        ]


viewAnimal : Animal -> Html msg
viewAnimal animal =
    let
        state : List Css.Style
        state =
            if Animal.isInteracting animal then
                [ Css.transform <| Css.translate2 (px 100) (px 0) ]

            else
                []

        enterAnimation : Css.Animations.Keyframes {}
        enterAnimation =
            Css.Animations.keyframes
                [ ( 0, [ View.CssExtra.maxWidthAnim 0 ] )
                , ( 100, [ View.CssExtra.maxWidthAnim 200 ] )
                ]
    in
    div
        [ css
            [ Css.flex (Css.int 1)
            , Transitions.transition [ Transitions.transform 1000 ]
            , flexCenter
            , Css.animationName enterAnimation
            , Css.animationDuration <| Css.ms 1500
            ]
        ]
        [ h3 [ css (Transitions.transition [ Transitions.transform 1000 ] :: state) ] [ text "Animal" ] ]


viewLoot : Int -> Item -> Html Msg
viewLoot index item =
    button [ Html.Styled.Events.onClick <| LootItem index ]
        [ text <| Engine.Item.iconString item ]


viewResource : Resource -> Html Msg
viewResource resource =
    div
        [ css
            [ Css.flex <| Css.int 1
            , View.Paper.paperGradient [ rgb 250 50 0, rgb 255 50 100 ]
            , flexCenter
            ]
        ]
        [ div []
            [ h3 [] [ text "Resource" ]
            , p []
                (if Resource.isAlive resource then
                    [ p [] [ text "alive" ]
                    ]

                 else if Resource.isRegrowing resource then
                    [ text "Regrowing "
                    ]

                 else if Resource.isHit resource then
                    [ p [ class "hit" ] [ text "hit" ]
                    ]

                 else if Resource.isEvade resource then
                    [ p [ class "evade" ] [ text "evade" ]
                    ]

                 else
                    case Resource.getLoot resource of
                        Just items ->
                            [ text "exhausted, loot"
                            , div [ css [ Css.displayFlex, View.CssExtra.gap 0.5 ] ] (List.indexedMap viewLoot items)
                            , button [ Html.Styled.Events.onClick ResetResource ] [ text "Done" ]
                            ]

                        Nothing ->
                            [ text "exhausted, loot"
                            , button [ Html.Styled.Events.onClick ResetResource ] [ text "Done" ]
                            ]
                )
            ]
        ]


locationStyle : Style
locationStyle =
    Css.batch
        [ Css.displayFlex
        , Css.flex3 (Css.int 1) (Css.int 1) (Css.int 0)
        , Css.minHeight (Css.rem 15)
        , Css.justifyContent Css.center
        ]


mainStyle : Attribute msg
mainStyle =
    css
        [ Css.minHeight <| Css.vh 100
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.stretch
        , Css.justifyContent Css.center
        , View.Paper.paperSolid <| rgb 255 255 200
        , Css.fontFamily Css.sansSerif
        ]


flexCenter : Style
flexCenter =
    Css.batch
        [ Css.displayFlex
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        ]


view : Model -> Html Msg
view model =
    main_
        [ id "app"
        , mainStyle
        ]
        [ div
            [ css [ locationStyle ] ]
            [ viewAnimal model.animal
            , viewResource model.resource
            ]
        , div
            [ css
                [ locationStyle
                , View.Paper.paperGradient [ rgb 130 50 70, rgb 30 50 100 ]
                , flexCenter
                ]
            ]
            [ viewInventory model.inventory ]
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
