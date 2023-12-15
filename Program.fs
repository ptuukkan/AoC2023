namespace AoC2023

open System.IO

module Program =
    [<EntryPoint>]
    let main args =
        let input = args[0] |> File.ReadAllText
        printf "%0A\n" (Day13.part2 input)
        0
