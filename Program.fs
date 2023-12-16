namespace AoC2023

open System.IO

module Program =
    [<EntryPoint>]
    let main args =
        // let input = @"c:\users\pektuu\projects\AoC2023\examples\day16" |> File.ReadLines
        let input = args[0] |> File.ReadLines
        printf "%0A\n" (Day16.part2 input)
        0
