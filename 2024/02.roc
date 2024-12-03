app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../package/main.roc",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

main =
    AoC.solve { year: 2024, day: 2, title: "Red-Nosed Reports", part1, part2 }

part1 : Str -> Result Str Str
part1 = \input ->
    count = input
        |> parseNumbers
        |> List.countIf safe

    Ok (Num.toStr count)
    
safe : List I64 -> Bool
safe = \levels ->
    diffs = List.map2 (List.dropFirst levels 1) (List.dropLast levels 1) (\a, b -> a - b)
    allNeg = List.all diffs \x -> Num.isNegative x && (Num.abs x >= 1 && Num.abs x <= 3)
    allPos = List.all diffs \x -> Num.isPositive x && (Num.abs x >= 1 && Num.abs x <= 3)
    allNeg || allPos

part2 : Str -> Result Str Str
part2 = \input ->
    count = input
        |> parseNumbers
        |> List.countIf actuallySafe

    Ok (Num.toStr count)

actuallySafe : List I64 -> Bool
actuallySafe = \levels ->
    List.range { start: At(0), end: Length(List.len levels) }
        |> List.map \i ->
            List.dropAt levels i
        |> List.any safe

parseNumbers : Str -> List List I64
parseNumbers = \input ->
    input
    |> Str.trim
    |> Str.splitOn "\n"
    |> List.map \s ->
        Str.splitOn s " "
        |> List.keepOks Str.toI64

expect
    part1 example == Ok "2"

expect
    part2 example == Ok "4"

example =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """
