port module Storage exposing (inventoryDecoder, saveInventory)

import Engine.Inventory exposing (Inventory, ItemStack)
import Engine.Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


port storeInventory : String -> Cmd msg



-- ENCODE


saveInventory : Inventory -> Cmd msg
saveInventory inventory =
    Encode.list itemStackEncoder inventory
        |> Encode.encode 0
        |> storeInventory


itemStackEncoder : ItemStack -> Value
itemStackEncoder stack =
    Encode.object
        [ ( "item", itemEncoder stack.item )
        , ( "amount", Encode.int stack.amount )
        ]


itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "icon", Encode.string <| String.fromChar item.icon )
        ]



-- DECODE


itemDecoder : Decoder Item
itemDecoder =
    let
        charHelper : String -> Decoder Char
        charHelper s =
            case String.toList s of
                [ c ] ->
                    Decode.succeed c

                _ ->
                    Decode.fail "Invalid char"
    in
    Decode.map Engine.Item.new
        (Decode.field "icon" Decode.string
            |> Decode.andThen charHelper
        )


itemStackDecoder : Decoder ItemStack
itemStackDecoder =
    Decode.map2 ItemStack
        (Decode.field "item" itemDecoder)
        (Decode.field "amount" Decode.int)


inventoryDecoder : Decoder Inventory
inventoryDecoder =
    Decode.list itemStackDecoder
