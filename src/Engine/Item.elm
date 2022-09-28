module Engine.Item exposing
    ( Item
    , iconString
    , new
    )


type alias Item =
    { icon : Char

    -- , name : String
    -- , description: String
    }


new : Char -> Item
new icon =
    Item icon


iconString : Item -> String
iconString item =
    item
        |> .icon
        |> String.fromChar
