app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.File
import One

readLines = \fileName ->
    str = File.readUtf8! fileName
    lines = Str.splitOn str "\n" |> List.dropLast 1
    dbg lines
    Task.ok lines

dayOne =
    lines = readLines! "data/day_one.txt"
    res = Result.try (One.partOne lines) \fst ->
        snd = One.partTwo? lines
        Ok (fst, snd)
    Task.fromResult res

main =
    when dayOne |> Task.result! is
        Ok (one, two) ->
            Num.toStr one |> Stdout.line!
            Num.toStr two |> Stdout.line!
        Err err ->
            dbg err
            Task.err (Exit 1 "something didn't work")
