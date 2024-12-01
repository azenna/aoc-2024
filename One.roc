module [ partOne, partTwo ]

parseLine =  \line ->
    nums = List.keepOks (Str.splitOn line "   ") \str ->
        Str.toI32 str
    fst  = List.get? nums 0
    snd  = List.get? nums 1
    Ok (fst, snd)

parseLines = \lines ->
    tuples = List.keepOks lines parseLine
    fst = List.map tuples (\tup -> tup.0) |> List.sortAsc
    snd = List.map tuples (\tup -> tup.1) |> List.sortAsc
    (fst, snd)

partOne = \lines ->

    (fst, snd) = parseLines lines
    List.map2 fst snd (\x, y -> Num.sub x y |> Num.abs)
    |> List.sum
    |> Ok

partTwo = \lines ->

    (fst, snd) = parseLines lines

    similarities =
        List.map fst \num ->
            List.countIf snd (\num2 -> num2 == num)

    List.map2 fst similarities (\x, y -> Num.mul (Num.toU64 x) (Num.toU64 y))
    |> List.sum
    |> Ok
