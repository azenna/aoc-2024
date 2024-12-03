module [ dayThree ]

import pf.File
import pf.Stdout

import parser.Parser exposing [Parser, many, oneOf, map, const]
import parser.String exposing [parseStr, codeunit, anyCodeunit, string, digits]

mulParser =
    const (\x -> \y -> Mul x y)
    |> Parser.skip (string "mul(")
    |> Parser.keep digits
    |> Parser.skip (string ",")
    |> Parser.keep digits
    |> Parser.skip (string ")")

doParser =
    const Do
    |> Parser.skip (string "do()")

dontParser =
    const Dont
    |> Parser.skip (string "don't")

contParser =
    oneOf [
        mulParser,
        doParser,
        dontParser,
        const Noop |> Parser.skip anyCodeunit,
    ]
    |> Parser.many

handleInstr = \state, instr ->
    when instr  is
        Do -> { state & flag: Bool.true }
        Dont -> { state & flag: Bool.false }
        Noop -> state
        Mul x y ->
            if state.flag then
                { state & total: state.total + x * y }
            else state


partOne = \cont ->
    parseStr contParser cont
    |> \r -> Result.withDefault r []
    |> \l -> List.map l \instr -> when instr is
        Mul x y -> x * y
        _ -> 0
    |> List.sum

partTwo = \cont ->
    parseStr contParser cont
    |> \r -> Result.withDefault r []
    |> \l -> List.walk l { total: 0, flag: Bool.true } handleInstr
    |> \s -> s.total

expect partOne "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" == 161

dayThree =
    cont = File.readUtf8! "data/day_three.txt"

    partOne cont
    |> Num.toStr
    |> Stdout.line!

    partTwo cont
    |> Num.toStr
    |> Stdout.line!
