namespace AoC2023

open System

module Day18 =

    let parse (input: string) =
        let split = input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        split[0], (int64 split[1])

    let parseHex (input: string) =
        let split = input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        let hexstr = split[2].Replace("(", "").Replace("#", "").Replace(")", "")
        let direction = match hexstr.Substring(5) with | "0" -> "R" | "1" -> "D" | "2" -> "L" | _ -> "U"
        let steps = Convert.ToInt64(hexstr.Substring(0, 5), 16)
        direction, steps

    let trace (path: (int64 * int64) list) ((dir, steps): string * int64) =
        let x, y = List.last path

        match dir with
        | "R" -> path @ [ x + steps, y ]
        | "D" -> path @ [ x, y + steps ]
        | "L" -> path @ [ x - steps, y ]
        | _ -> path @ [ x, y - steps ]
        
    let perimeter (vertices: (int64 * int64) list) =
        vertices
        |> Seq.pairwise
        |> Seq.map (fun ((x1, y1), (x2, y2)) -> abs (x2 - x1) + abs (y2 - y1))
        |> Seq.sum
        
    let area (vertices: (int64 * int64) list) =
        vertices
        |> Seq.pairwise
        |> Seq.map (fun ((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1)
        |> Seq.sum
        |> abs
        |> (fun x -> x / 2L)

    let part1 (input: string seq) =
        let vertices =
            input
            |> Seq.map parse
            |> Seq.fold trace [ 0L, 0L ]
        2L
        |> (/) (perimeter vertices)
        |> (+) (area vertices)
        |> (+) 1L

    let part2 (input: string seq) =
        let vertices =
            input
            |> Seq.map parseHex
            |> Seq.fold trace [ 0, 0 ]
        2L
        |> (/) (perimeter vertices)
        |> (+) (area vertices)
        |> (+) 1L
