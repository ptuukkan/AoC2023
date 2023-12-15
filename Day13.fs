namespace AoC2023

open System

module Day13 =
    type Direction =
        | Horizontal
        | Vertical

    let parseInput (input: string) =
        input.Split([| "\r\n\r\n"; "\n\n"; "\r\r" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (_.Split([| "\r\n"; "\n"; "\r" |], StringSplitOptions.RemoveEmptyEntries))

    let parseInputP2 (input: string) =
        input.Split([| "\r\n\r\n"; "\n\n"; "\r\r" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (_.Split([| "\r\n"; "\n"; "\r" |], StringSplitOptions.RemoveEmptyEntries))
        |> Array.map (Array.map (_.ToCharArray()))

    let findPossibleMirrors ((direction, pattern): Direction * string array) =
        let possibleMirrors =
            pattern
            |> Array.indexed
            |> Array.pairwise
            |> Array.filter (fun ((i1, s1), (i2, s2)) -> s2.Equals(s1))
            |> Array.map (fun ((i1, s1), (i2, s2)) -> (i1, i2))

        direction, pattern, possibleMirrors

    let findPossibleMirrorsPart2 ((direction, pattern): Direction * char array array) =
        let possibleMirrors =
            pattern
            |> Array.indexed
            |> Array.pairwise
            |> Array.filter (fun ((i1, s1), (i2, s2)) ->
                let mutable diffs = 0

                s1
                |> Array.iteri (fun i c ->
                    if c <> s2[i] then
                        diffs <- diffs + 1)

                diffs <= 1)
            |> Array.map (fun ((i1, s1), (i2, s2)) -> (i1, i2))

        direction, pattern, possibleMirrors

    let verifyMirrors ((direction, pattern, possibleMirrors): Direction * string array * (int * int) array) =
        let mirror =
            possibleMirrors
            |> Array.filter (fun (i1, i2) ->
                let mirrorLength = min i2 ((pattern.Length - 1) - i1)

                [ 0 .. mirrorLength - 1 ]
                |> List.map (fun i -> pattern[i1 - i] = pattern[i2 + i])
                |> List.forall id)
            |> Array.tryHead

        direction, mirror

    let verifyMirrorsPart2 ((direction, pattern, possibleMirrors): Direction * char array array * (int * int) array) =
        let mirror =
            possibleMirrors
            |> Array.filter (fun (i1, i2) ->
                let mirrorLength = min i2 ((pattern.Length - 1) - i1)
                let mutable dirt = 0

                [ 0 .. mirrorLength - 1 ]
                |> List.map (fun i ->
                    pattern[i1 - i]
                    |> Array.iteri (fun j c ->
                        if c <> pattern[i2 + i][j] then
                            dirt <- dirt + 1)

                    dirt <= 1)
                |> List.forall id
                |> (fun x -> x && dirt = 1))
            |> Array.tryHead

        direction, mirror

    let calculateValue ((direction, mirror): Direction * (int * int) option) =
        mirror
        |> Option.map (fun (i1, i2) ->
            match direction with
            | Horizontal -> direction, i2
            | Vertical -> direction, i2)

    let sumValues ((horizontals, verticals): (Direction * int) array * (Direction * int) array) =
        horizontals
        |> Array.map snd
        |> Array.sum
        |> (*) 100
        |> (+) (verticals |> Array.map snd |> Array.sum)

    let part1 (input: string) =
        let horizontal = input |> parseInput |> Array.map (fun a -> Horizontal, a)

        let vertical =
            input
            |> parseInput
            |> Array.map (Array.map (_.ToCharArray()))
            |> Array.map Array.transpose
            |> Array.map (fun a -> (Vertical, a |> Array.map (fun x -> String.Join("", x))))

        horizontal
        |> Array.append vertical
        |> Array.map findPossibleMirrors
        |> Array.map verifyMirrors
        |> Array.map calculateValue
        |> Array.choose id
        |> Array.partition (fun (a, b) -> a = Horizontal)
        |> sumValues

    let part2 (input: string) =
        let horizontal = input |> parseInputP2 |> Array.map (fun a -> Horizontal, a)

        let vertical =
            input
            |> parseInput
            |> Array.map (Array.map (_.ToCharArray()))
            |> Array.map Array.transpose
            |> Array.map (fun a -> (Vertical, a))

        horizontal
        |> Array.append vertical
        |> Array.map findPossibleMirrorsPart2
        |> Array.map verifyMirrorsPart2
        |> Array.map calculateValue
        |> Array.choose id
        |> Array.partition (fun (a, b) -> a = Horizontal)
        |> sumValues
