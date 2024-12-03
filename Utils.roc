module [ readLines ]

import pf.File

readLines = \fileName ->
    str = File.readUtf8! fileName
    lines = Str.splitOn str "\n" |> List.dropLast 1
    Task.ok lines
