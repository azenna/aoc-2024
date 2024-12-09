module [ daySeven ]

import Utils
import pf.Stdout

daySeven =
    lines = Utils.readLines! "data/day_seven.txt"
    parsed = List.keepOks lines \line ->
        {before, after} = Str.splitFirst? line ": "
        testValue = Str.toI64? before
        numbers = List.keepOks (Str.splitOn after " ") \value -> Str.toI64 value
        Ok (testValue, numbers)

    trueEquations = List.keepIf parsed isTrueEquation
    trueEquations2 = List.keepIf parsed isTrueEquation2

    out = \eqs ->
        List.map eqs \eq -> eq.0
        |> List.sum
        |> Num.toStr
        |> Stdout.line

    out! trueEquations
    out  trueEquations2

isTrueEquation = \(testValue, eq) ->
    rec = \recEq, acc -> when recEq is
        [] -> acc == testValue
        [x, .. as xs ] -> rec xs (acc + x) || rec xs (acc * x)
    rec (List.dropFirst eq 1) (Result.withDefault (List.first eq) 0)

isTrueEquation2 = \(testValue, eq) ->
    rec = \recEq, acc -> when recEq is
        [] -> acc == testValue
        [x, .. as xs ] -> rec xs (acc + x) || rec xs (acc * x) || rec xs (Result.withDefault (Str.toI64 "$(Num.toStr acc)$(Num.toStr x)") 0)
    rec (List.dropFirst eq 1) (Result.withDefault (List.first eq) 0)
