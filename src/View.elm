module View exposing (viewAnimal, viewInventory, viewLocation, viewResource)

import Engine.Animal exposing (Animal)
import Engine.Inventory exposing (Inventory, ItemStack)
import Engine.Item as Item exposing (Item)
import Engine.Resource as Resource exposing (Resource)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


viewAnimal : Animal -> Html msg
viewAnimal _ =
    div [] [ text "Animal" ]


viewLoot : (Int -> msg) -> Int -> Item -> Html msg
viewLoot lootEvent index item =
    button [ onClick (lootEvent index) ] [ text <| Item.iconString item ]


viewResource : (Int -> msg) -> msg -> Resource -> Html msg
viewResource lootEvent resetEvent resource =
    case Resource.getLoot resource of
        Just loot ->
            div []
                [ div [] (List.indexedMap (viewLoot lootEvent) loot)
                , button [ onClick resetEvent ] [ text "Done" ]
                ]

        Nothing ->
            div [] [ text "Resource" ]


viewItemStack : ItemStack -> Html msg
viewItemStack stack =
    div [] [ text (String.fromChar stack.item.icon ++ "x" ++ String.fromInt stack.amount) ]


viewInventory : Inventory -> Html msg
viewInventory inventory =
    div [] (List.map viewItemStack inventory)


viewLocation : List (Html msg) -> Html msg
viewLocation content =
    div [ class "location" ] content
