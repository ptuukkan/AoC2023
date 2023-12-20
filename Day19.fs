namespace AoC2023

open System
open System.Collections.Generic

module Day19 =
    type Part = { X: int; M: int; A: int; S: int }

    type PartRange =
        { X: int * int
          M: int * int
          A: int * int
          S: int * int
          Dst: string }

    let toParts (input: string array) : Part array =
        input
        |> Array.map (_.Replace("{", "").Replace("}", "").Split(","))
        |> Array.map (Array.map (_.Split("=")))
        |> Array.map (fun p ->
            { X = p[0][1] |> int
              M = p[1][1] |> int
              A = p[2][1] |> int
              S = p[3][1] |> int })

    let toWorkflows (input: string array) =

        (Map.empty<string, string array>, input)
        ||> Array.fold (fun workflows line ->
            let key = line.Substring(0, line.IndexOf('{'))

            let rules = line.Substring(line.IndexOf('{') + 1).Replace("}", "").Split(',')

            workflows.Add(key, rules))

    let parse (input: string) =
        let split =
            input.Split([| "\r\n\r\n"; "\n\n"; "\r\r" |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (_.Split([| "\r\n"; "\n"; "\r" |], StringSplitOptions.RemoveEmptyEntries))

        split[0] |> toWorkflows, split[1] |> toParts

    let rec sort (workflows: Map<string, string array>) (current: string) (part: Part) =
        let destination =
            workflows[current]
            |> Array.filter (fun rule ->
                let colon = rule.IndexOf ':'

                if colon = -1 then
                    true
                else
                    let target = rule.Substring(0, 1)
                    let comp = rule.Substring(1, 1)
                    let value = rule.Substring(2, colon - 2) |> int

                    match target, comp with
                    | "a", "<" -> part.A < value
                    | "a", ">" -> part.A > value
                    | "m", "<" -> part.M < value
                    | "m", ">" -> part.M > value
                    | "s", "<" -> part.S < value
                    | "s", ">" -> part.S > value
                    | "x", "<" -> part.X < value
                    | "x", ">" -> part.X > value
                    |_ , _ -> raise (Exception()))
            |> Array.head
            |> (fun rule -> rule.Substring(rule.IndexOf(':') + 1))

        match destination with
        | "A" -> true
        | "R" -> false
        | _ -> sort workflows destination part

    // let rec sortRange (workflows: Map<string, string array>) (current: string) (accepted: (string * int * int) array) (ranges: (string * int * int) array) =
    //     match ranges |> Array.tryHead with
    //     | None -> Return accepted
    //     | Some(label, start, stop) ->
    //         let rules =
    //             workflows[current]
    //             |> Array.filter (fun rule ->
    //                 rule.IndexOf(':') = -1 || rule.Substring(0,1) = label)

    let processRule (part: PartRange) (rule: string) =
        if rule.IndexOf(':') <> -1 then
            let target = rule.Substring(0, 1)
            let comp = rule.Substring(1, 1)
            let value = rule.Substring(2, rule.IndexOf(':') - 2) |> int
            let newDst = rule.Substring(rule.IndexOf(':') + 1)

            match comp, target with
            | ">", "a" when fst part.A > value -> (Some { part with Dst = newDst }, None)
            | ">", "m" when fst part.M > value -> (Some { part with Dst = newDst }, None)
            | ">", "s" when fst part.S > value -> (Some { part with Dst = newDst }, None)
            | ">", "x" when fst part.X > value -> (Some { part with Dst = newDst }, None)
            | ">", "a" when snd part.A <= value -> (None, Some part)
            | ">", "m" when snd part.M <= value -> (None, Some part)
            | ">", "s" when snd part.S <= value -> (None, Some part)
            | ">", "x" when snd part.X <= value -> (None, Some part)
            | "<", "a" when snd part.A < value -> (Some { part with Dst = newDst }, None)
            | "<", "m" when snd part.M < value -> (Some { part with Dst = newDst }, None)
            | "<", "s" when snd part.S < value -> (Some { part with Dst = newDst }, None)
            | "<", "x" when snd part.X < value -> (Some { part with Dst = newDst }, None)
            | "<", "a" when fst part.A >= value -> (None, Some part)
            | "<", "m" when fst part.M >= value -> (None, Some part)
            | "<", "s" when fst part.S >= value -> (None, Some part)
            | "<", "x" when fst part.X >= value -> (None, Some part)
            | ">", "a" ->
                (Some
                    { part with
                        A = value + 1, snd part.A
                        Dst = newDst },
                 Some { part with A = fst part.A, value })
            | "<", "a" ->
                (Some
                    { part with
                        A = fst part.A, value - 1
                        Dst = newDst },
                 Some { part with A = value, snd part.A })
            | ">", "m" ->
                (Some
                    { part with
                        M = value + 1, snd part.M
                        Dst = newDst },
                 Some { part with M = fst part.M, value })
            | "<", "m" ->
                (Some
                    { part with
                        M = fst part.M, value - 1
                        Dst = newDst },
                 Some { part with M = value, snd part.M })
            | ">", "x" ->
                (Some
                    { part with
                        X = value + 1, snd part.X
                        Dst = newDst },
                 Some { part with X = fst part.X, value })
            | "<", "x" ->
                (Some
                    { part with
                        X = fst part.X, value - 1
                        Dst = newDst },
                 Some { part with X = value, snd part.X })
            | ">", "s" ->
                (Some
                    { part with
                        S = value + 1, snd part.S
                        Dst = newDst },
                 Some { part with S = fst part.S, value })
            | "<", "s" ->
                (Some
                    { part with
                        S = fst part.S, value - 1
                        Dst = newDst },
                 Some { part with S = value, snd part.S })
            |_ , _ -> raise (Exception())

        else
            (Some {part with Dst = rule }, None)

    let dothing (workflows: Map<string, string array>) =
        let stack = Stack<PartRange>()
        let accepted = Stack<PartRange>()

        stack.Push(
            { A = 1, 4000
              M = 1, 4000
              S = 1, 4000
              X = 1, 4000
              Dst = "in" }
        )

        while stack.TryPeek() |> fst do
            let part = stack.Pop()

            let rules = workflows[part.Dst]

            (([], Some(part)), rules)
            ||> Array.fold (fun (processed, queue) rule ->
                match queue with
                | Some(part) ->
                    let new_p, new_q = processRule part rule

                    match new_p with
                    | Some p -> p :: processed, new_q
                    | None -> processed, new_q
                | None -> (processed, queue))
            |> fst
            |> List.iter (fun part ->
                match part.Dst with
                | "A" ->
                    accepted.Push(part)
                | "R" ->
                    ()
                | _ ->
                    stack.Push(part))

        seq {
            for x in accepted do
                yield x
        }

    let filterCombinations (accepted: (string * int * int) array) (rules: string array) =
        rules |> Array.filter (fun x -> x.IndexOf(':') <> -1)


    let part1 (input: string) =
        let workflows, parts = input |> parse

        parts
        |> Array.filter (sort workflows "in")
        |> Array.map (fun part -> part.A + part.M + part.S + part.X)
        |> Array.sum

    let diff ((a, b): int * int) = b - a + 1 |> uint64

    let part2 (input: string) =
        input
        |> parse
        |> fst
        |> dothing
        |> Seq.fold (fun acc part ->
            diff part.A * diff part.M * diff part.X * diff part.S
            |> (+) acc ) 0UL
        // |> Seq.fold
        //     (fun (a, m, s, x) part ->
        //         (a @ [ fst part.A .. snd part.A ],
        //          m @ [ fst part.M .. snd part.M ],
        //          s @ [ fst part.S .. snd part.S ],
        //          x @ [ fst part.X .. snd part.X ]))
        //     ([], [], [], [])
        // |> (fun (a, m, s, x) -> [a; m; s; x])
        // |> List.map List.distinct
        // |> List.map List.length
        // |> List.map uint64
        // |> List.reduce (*)
// |> Seq.map (fun part -> )
// |> Seq.groupBy fst
// |> Seq.map (fun (g, n) -> n |> Seq.map snd)
// |> Seq.map Seq.sum
