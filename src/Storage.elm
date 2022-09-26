port module Storage exposing (saveInventory)

import Engine.Item exposing (Item)
import Json.Encode as Encode exposing (Value)


port storeInventory : String -> Cmd msg


itemEncoder : Item -> Value
itemEncoder item =
    Encode.string <| String.fromChar item


saveInventory : List Item -> Cmd msg
saveInventory items =
    Encode.list itemEncoder items
        |> Encode.encode 0
        |> storeInventory
