module [dayFour]

import Utils
import pf.Stdout

dayFour =
    lines = Utils.readLines! "data/day_four.txt"
    lines
    |> partOne
    |> Num.toStr
    |> Stdout.line!

    lines
    |> partTwo
    |> Num.toStr
    |> Stdout.line!

getStarts = \char, board ->
    List.mapWithIndex board \cs, i ->
        lineStarts = List.mapWithIndex cs \c, j ->
            if c == char then
                Ok (i, j)
            else
                Err Nothing
        List.keepOks lineStarts \r -> r
    |> List.joinMap \l -> l


makeMove = \(x, y), (dirx, diry) ->
    i = Num.toU64Checked? (Num.toI64 x + dirx)
    j = Num.toU64Checked? (Num.toI64 y + diry)
    Ok (i, j)

getAt = \i, j, board ->
    List.get? board i
    |> (\l -> List.get l j)

getRelative = \cur, dir, board ->
    (i, j) = makeMove? cur dir
    getAt i j board


partOne = \lines ->
    board = List.map lines Str.toUtf8

    starts = getStarts 'X' board

    search = \char, cur, dir ->
        when char is
            'S' -> Ok Xmas
            _ ->
                (i, j) = makeMove? cur dir
                actNext = getAt? i j board

                shouldNext =
                    if char == 'X' then
                        'M'
                    else if char == 'M' then
                        'A'
                    else
                        'S'

                if actNext == shouldNext then
                    search actNext (i, j) dir
                else
                    Err Nothing

    dirs = List.joinMap [1, 0, -1] \x ->
        List.keepOks [1, 0, -1] \y ->
            if x == 0 && y == 0 then
                Err Nothing
            else
                Ok (x, y)

    matches = List.joinMap dirs \dir ->
        List.keepOks starts \start ->
            search 'X' start dir

    List.len matches

partTwo = \lines ->
    board = List.map lines Str.toUtf8
    starts = getStarts 'A' board

    otherCorner = \char ->
        when char is
            'M' -> Ok 'S'
            'S' -> Ok 'M'
            _ -> Err Nothing

    search = \cur ->
        topLeft = getRelative? cur (-1, -1) board
        botRight = getRelative? cur (1, 1) board

        topRight  = getRelative? cur (-1, 1) board
        botLeft = getRelative? cur (1, -1) board

        dbg (topLeft, botRight)
        dbg (topRight, botLeft)

        if otherCorner? topLeft == botRight && otherCorner? topRight == botLeft then
            dbg "got here"
            Ok Xmas
        else
            Err Nothing

    List.keepOks starts search
    |> List.len
