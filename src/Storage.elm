port module Storage exposing (decodeStoredInventory, saveInventory)

import Engine.Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


port storeInventory : String -> Cmd msg


itemEncoder : Item -> Value
itemEncoder item =
    Encode.string <| Engine.Item.toString item


itemHelp : String -> Decoder Item
itemHelp itemString =
    case Engine.Item.fromString itemString of
        Just item ->
            Decode.succeed item

        Nothing ->
            Decode.fail <|
                "Invalid item data"


itemDecoder : Decoder Item
itemDecoder =
    Decode.string
        |> Decode.andThen itemHelp


inventoryDecoder : Decoder (List Item)
inventoryDecoder =
    Decode.list itemDecoder


saveInventory : List Item -> Cmd msg
saveInventory items =
    Encode.list itemEncoder items
        |> Encode.encode 0
        |> storeInventory


decodeStoredInventory : String -> Result String (List Item)
decodeStoredInventory inventoryJson =
    Decode.decodeString inventoryDecoder inventoryJson
        |> Result.mapError (always "Error decoding items")
