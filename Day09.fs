namespace AoC2023

module Day09 =

    let calculateDiffs (history: int array) =
        let rec calculateDiffsInner (sequences: int array list) =
            match List.last sequences with
            | zeroes when zeroes |> Array.forall (fun x -> x = 0) -> sequences
            | notZeroes ->
                let newLine = notZeroes |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
                calculateDiffsInner (sequences @ [ newLine ])

        calculateDiffsInner [ history ]

    let extrapolate (sequences: int array list) =
        sequences
        |> List.rev
        |> List.fold (fun a b -> Array.last b + a) 0
    
    let extrapolateBackwards (sequences: int array list) =
        sequences
        |> List.rev
        |> List.fold (fun a b -> Array.head b - a) 0

    let part1 (input: string seq) =
        input
        |> Seq.map (_.Split(" ") >> Array.map int)
        |> Seq.map calculateDiffs
        |> Seq.map extrapolate
        |> Seq.sum
        
    let part2 (input: string seq) =
        input
        |> Seq.map (_.Split(" ") >> Array.map int)
        |> Seq.map calculateDiffs
        |> Seq.map extrapolateBackwards
        |> Seq.sum