namespace AoC2023

open System.IO

module Program =
    [<EntryPoint>]
    let main args =
        let input = args[0] |> File.ReadLines
        printf "%0A\n" (Day05.part2 input)
        0
