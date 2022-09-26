module Engine.Item exposing (Item(..), fromString, toString)


type Item
    = Coconut
    | Strawberry


toString : Item -> String
toString item =
    case item of
        Coconut ->
            "Coconut"

        Strawberry ->
            "Strawberry"


fromString : String -> Maybe Item
fromString string =
    case string of
        "Coconut" ->
            Just Coconut

        "Strawberry" ->
            Just Strawberry

        _ ->
            Nothing
