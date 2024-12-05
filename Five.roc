module [ dayFive ]

import pf.Stdout
import pf.File
import Utils

import parser.Parser
import parser.String exposing [parseStr, codeunit, anyCodeunit, string, digits]

dayFive =
    cont = File.readUtf8! "data/day_five.txt"
    parsed = Task.fromResult! (parse cont)

    partOne parsed
    |> Num.toStr
    |> Stdout.line!

    partTwo parsed
    |> Num.toStr
    |> Stdout.line!

parse = \cont ->
    { before, after } = Str.splitFirst? cont "\n\n"

    rules =
        Parser.const (\x -> \y -> (x, y))
        |> Parser.keep digits
        |> Parser.skip (string "|")
        |> Parser.keep digits
        |> Parser.skip (Parser.maybe (string "\n"))
        |> Parser.many
        |> (\parser -> parseStr parser before)?

    updates =
        digits
        |> Parser.skip (Parser.maybe (string ","))
        |> Parser.many
        |> Parser.skip (string "\n")
        |> Parser.many
        |> (\parser -> parseStr parser after)?

    Ok (rules, updates)

makeRules : List (U64, U64) -> Dict U64 (List U64)
makeRules = \rules ->
    List.walk rules (Dict.empty {}) \dict, (k, r) ->
        Dict.update dict k \res -> when res is
            Err Missing -> Ok [r]
            Ok rs -> Ok (List.append rs r)


middle = \ls ->
    List.get ls (((List.len ls) - 1) // 2)

checkRule = \rulesDict, x, update ->
    rule = Dict.get? rulesDict x
    {before, after } = List.splitFirst? update x
    Ok (List.all before \y -> List.contains rule y |> Bool.not)

checkUpdate =  \rulesDict, update ->
    List.all update \x -> when checkRule rulesDict x update is
        Ok b -> b
        Err _ -> Bool.true

partOne = \(rules, updates) ->
    rulesDict = makeRules rules
    compliant = List.keepIf updates \update -> checkUpdate rulesDict update
    List.keepOks compliant middle
    |> List.sum

partTwo = \(rules, updates) ->
    rulesDict = makeRules rules
    nonCompliant = List.keepIf updates \update -> checkUpdate rulesDict update |> Bool.not

    occursBefore : U64, U64 -> Bool
    occursBefore = \a,b ->
        List.contains (Result.withDefault (Dict.get rulesDict a) []) b

    quickSort = \update ->
        when update is
            [] -> []
            _ ->
                pivot = Result.withDefault (middle update) 0
                less = List.keepIf update \x -> occursBefore x pivot
                great = List.keepIf update \x -> occursBefore pivot x
                List.concat (List.append (quickSort less) pivot) (quickSort great)

    fixed = List.map nonCompliant quickSort
    List.keepOks fixed middle
    |> List.sum
