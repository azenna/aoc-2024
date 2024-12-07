module [ daySix ]

import Utils
import pf.Stdout

daySix =
    lines = Utils.readLines! "data/day_six.txt"
    parsed : (Set (I64, I64), (I64, I64))
    parsed = List.walkWithIndex lines (Set.empty {}, (0, 0)) \acc, line, i ->
        Str.walkUtf8WithIndex line acc \(set, start), c, j ->
            ind = (Num.toI64 i, Num.toI64 j)
            when c is
            '#' -> (Set.insert set ind, start)
            '^' -> (set, ind)
            _ -> (set, start)

    partOne parsed.0 parsed.1 (Num.toI64 (List.len lines))
    |> Num.toStr
    |> Stdout.line!

    partTwo parsed.0 parsed.1 (Num.toI64 (List.len lines))
    |> Num.toStr
    |> Stdout.line!

rot90 = \(x, y) -> (y, x * -1)

isOutOfBounds = \(x, y), dim ->
    x >= dim || y >= dim || x < 0 || y < 0

partOne = \set, start, dim ->
    rec = \cur, dir, visited, acc ->
        next = (cur.0 + dir.0, cur.1 + dir.1)
        if isOutOfBounds next dim then
            acc + 1
        else if Set.contains set next then
            rec cur (rot90 dir) visited acc
        else if Set.contains visited next then
            rec next dir visited acc
        else
            rec next dir (Set.insert visited next) (acc + 1)
    rec start (-1,0) (Set.empty {}) 0

partTwo = \set, start, dim ->

    tryLoop = \cur, dir, new, visited ->
        next = (cur.0 + dir.0, cur.1 + dir.1)
        if isOutOfBounds next dim then
            Bool.false
        else if Set.contains visited (cur, dir) then
            Bool.true
        else if Set.contains set next || new == next then
            tryLoop cur (rot90 dir) new visited
        else
            tryLoop next dir new (Set.insert visited (cur, dir))

    rec = \cur, dir, acc ->
        next = (cur.0 + dir.0, cur.1 + dir.1)
        if isOutOfBounds next dim then
            acc
        else if Set.contains set next then
            rec cur (rot90 dir) acc
        else if tryLoop cur dir next (Set.empty {}) then
            rec next dir (Set.insert acc next)
        else
            rec next dir acc

    Set.len (rec start (-1,0) (Set.empty {}))
