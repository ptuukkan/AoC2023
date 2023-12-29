namespace AoC2023

open System.IO

module Program =
    [<EntryPoint>]
    let main args =
        // let input = @"c:\users\pektuu\projects\AoC2023\inputs\day12" |> File.ReadLines
        let input = args[0] |> File.ReadLines
        printf "%0A\n" (Day24.part1 input)
        0
