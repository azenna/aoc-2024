module [dayFour]

import Utils
import pf.Stdout

dayFour =
    lines = Utils.readLines! "data/day_four.txt"
    lines
    |> partOne
    |> Num.toStr
    |> Stdout.line!

partOne = \lines ->
    parse = List.map lines Str.toUtf8

    starts =
        List.mapWithIndex parse \cs, i ->
            lineStarts = List.mapWithIndex cs \c, j ->
                if c == 'X' then
                    Ok (i, j)
                else
                    Err Nothing
            List.keepOks lineStarts \r -> r
        |> List.joinMap \l -> l

    search = \char, cur, dir ->
        when char is
            'S' -> Ok Xmas
            _ ->

                i = Num.toU64Checked? (Num.toI64 cur.0 + dir.0)
                j = Num.toU64Checked? (Num.toI64 cur.1 + dir.1)

                hold = List.get? parse i
                actNext = List.get? parse i |> (\l -> List.get l j)?

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
