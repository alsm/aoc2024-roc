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
    AoC.solve { year: 2024, day: 1, title: "Historian Hysteria", part1, part2 }

part1 : Str -> Result Str Str
part1 = \input ->
    nums = unwrap (parseNumbers input)
    left =
        nums
        |> List.map (\x -> x.first)
        |> List.sortAsc
    right =
        nums
        |> List.map (\x -> x.second)
        |> List.sortAsc

    Ok (Num.toStr (List.map2 left right (\a, b -> Num.absDiff a b) |> List.sum))

part2 : Str -> Result Str Str
part2 = \input ->
    nums = unwrap (parseNumbers input)
    similarity =
        nums
        |> List.map (\x -> x.second)
        |> Set.fromList
        |> Set.toList
        |> List.map (\x -> (x, List.countIf nums (\y -> y.second == x)))
        |> Dict.fromList

    Ok (Num.toStr (List.map nums (\x -> x.first * Result.withDefault (Dict.get similarity x.first) 0) |> List.sum))

parseNumbers : Str -> Result (List { first : U64, second : U64 }) _
parseNumbers = \input ->
    input
        |> Str.splitOn "\n"
        |> List.mapTry \l ->
            x =
                Str.splitOn l "   "
                |> List.mapTry Str.toU64
                |> try
            f = List.first? x
            s = List.last? x
            Ok { first: f, second: s }

unwrap = \result ->
    when result is
        Ok a -> a
        Err _ -> crash "oops"

expect
    part1 example == Ok "11"

expect
    part2 example == Ok "31"

example =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """
