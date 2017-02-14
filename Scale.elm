module Scale exposing (min, max)


min list =
    case (List.minimum list) of
        Nothing ->
            0

        Just x ->
            x


max list =
    case (List.maximum list) of
        Nothing ->
            0

        Just x ->
            x
