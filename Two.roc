module [ dayTwo ]

import Utils
import pf.Stdout

dayTwo =
    lines = Utils.readLines! "data/day_two.txt"
    partOne lines |> Num.toStr |> Stdout.line!
    partTwo lines |> Num.toStr |> Stdout.line!

parseLine = \line ->
    List.keepOks (Str.splitOn line " ") Str.toI32

changes = \nums ->
    List.map2 nums (List.dropFirst nums 1) \x, y ->
        Num.sub x y

isIncreasing = \cs ->
    List.all cs \change ->
        change >= 1 && change <= 3

isDecreasing = \cs ->
    List.all cs \change ->
        change <= -1 && change >= -3

isSafe = \cs ->
    isIncreasing cs || isDecreasing cs

partOne = \lines ->
    safeLines = List.keepIf lines \line ->
        parseLine line
        |> changes
        |> isSafe
    List.len safeLines

partTwo = \lines ->
    safeLines = List.keepIf lines \line ->
        nums = parseLine line
        potentialLevels = List.mapWithIndex nums \_, i -> List.dropAt nums i
        List.any potentialLevels \levels ->
            changes levels
            |> isSafe
    List.len safeLines

expect partOne
    [ "7 6 4 2 1"
    , "1 2 7 8 9"
    , "9 7 6 2 1"
    , "1 3 2 4 5"
    , "8 6 4 4 1"
    , "1 3 6 7 9" ] == 2

expect partTwo
    [ "7 6 4 2 1"
    , "1 2 7 8 9"
    , "9 7 6 2 1"
    , "1 3 2 4 5"
    , "8 6 4 4 1"
    , "1 3 6 7 9" ] == 4
