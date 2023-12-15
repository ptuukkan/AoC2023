namespace AoC2023

open System
open System.Collections

module Day15 =
    type Operation =
        | Dash
        | Equals of int

    type Sequence = { Label: string; Operation: Operation }

    type Lens = { Label: string; FocalLength: int }

    let parse (str: string) =
        let split = str.Split([| "="; "-" |], StringSplitOptions.RemoveEmptyEntries)

        { Label = split[0]
          Operation =
            match Array.tryItem 1 split with
            | Some v -> Equals(int v)
            | None -> Dash }

    let hash (str: string) =
        (0, str.Trim().ToCharArray())
        ||> Array.fold (fun value c -> (int c + value) * 17 % 256)

    let initialize (boxes: Map<int, Lens list>) (sequence: Sequence) =
        match sequence.Operation, hash sequence.Label with
        | Dash, boxNo ->
            boxes
            |> Map.change boxNo (fun box ->
                match box with
                | Some lenses -> lenses |> List.filter (fun l -> l.Label <> sequence.Label) |> Some
                | None -> None)
        | Equals focalLength, boxNo ->
            if boxes |> Map.containsKey boxNo then
                boxes
                |> Map.change boxNo (fun box ->
                    match box with
                    | Some lenses ->
                        let newLenses =
                            lenses
                            |> List.map (fun x ->
                                if x.Label = sequence.Label then
                                    { x with FocalLength = focalLength }
                                else
                                    x)

                        if newLenses |> List.exists (fun x -> x.Label = sequence.Label) then
                            newLenses
                        else
                            newLenses
                            @ [ { Label = sequence.Label
                                  FocalLength = focalLength } ]
                        |> Some
                    | None -> None)
            else
                boxes
                |> Map.add
                    boxNo
                    [ { Label = sequence.Label
                        FocalLength = focalLength } ]

    let calculateFocusingPower (power: int) (boxNo: int) (lenses: Lens list) =
        lenses
        |> List.mapi (fun i lens -> (1 + boxNo) * (i + 1) * lens.FocalLength)
        |> List.sum
        |> (+) power

    let part1 (input: string) =
        input.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map hash
        |> Array.sum

    let part2 (input: string) =
        input.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parse
        |> Array.fold initialize Map.empty
        |> Map.fold calculateFocusingPower 0
