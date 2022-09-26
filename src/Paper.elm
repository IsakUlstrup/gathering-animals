module Paper exposing (paperGradient)

import Css exposing (Color, Style, batch)


colorToString : Color -> String
colorToString color =
    let
        channelString c =
            String.fromInt c

        channels =
            [ color.red, color.green, color.blue ] |> List.map channelString |> List.intersperse ", " |> String.concat
    in
    "rgb(" ++ channels ++ ")"


paperGradient : List Color -> Style
paperGradient colors =
    let
        colorStops =
            List.map colorToString colors |> List.intersperse ", " |> String.concat
    in
    batch
        [ Css.property "background-image" (textureUrl ++ ", linear-gradient( to bottom right, " ++ colorStops ++ ")")
        ]


textureUrl : String
textureUrl =
    " url('data:image/png;base64,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')"
