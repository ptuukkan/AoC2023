namespace AoC2023

open System
open System.Text.RegularExpressions

module Day08 =
    let parseInput (input: string seq) =
        let instructions = input |> Seq.head |> _.ToCharArray()

        let lines =
            input
            |> Seq.skip 2
            |> Seq.map (fun line -> Regex.Split(line, "[\s\=\(\)\,]+"))
            |> Seq.map (Array.filter (fun x -> x <> ""))

        let nodes =
            (Map.empty<string, string * string>, lines)
            ||> Seq.fold (fun map line -> map.Add(line[0], (line[1], line[2])))

        instructions, nodes

    let rec walk
        (instructions: char array)
        (index: int)
        (nodes: Map<string, string * string>)
        (currentNode: string)
        (steps: int)
        =
        if currentNode.EndsWith('Z') then
            steps
        else
            match instructions |> Array.tryItem index with
            | Some instruction ->
                let newNode =
                    match instruction with
                    | 'R' -> snd nodes[currentNode]
                    | 'L' -> fst nodes[currentNode]
                    | _ -> raise (Exception("Unrecognized instruction"))

                walk instructions (index + 1) nodes newNode (steps + 1)
            | None -> walk instructions 0 nodes currentNode steps

    let getPrimes (num: int) =
        let rec getPrimesInner num prime primes =
            if num = prime then
                prime :: primes
            elif num % prime = 0 then
                getPrimesInner (num / prime) prime (prime :: primes)
            else
                getPrimesInner num (prime + 1) primes

        getPrimesInner num 2 []

    let part1 (input: string seq) =
        let instructions, nodes = parseInput input

        walk instructions 0 nodes "AAA" 0

    let getCommonPrimes (primes: int list array) =
        let len = Array.length primes

        primes
        |> List.concat
        |> List.groupBy id
        |> List.map snd
        |> List.map (fun x ->
            match List.length x with
            | a when a >= len -> [ 1 .. ((a / len) + (a % len)) ] |> List.map (fun c -> x |> List.head)
            | _ -> x)
        |> List.concat

    let part2 (input: string seq) =
        let instructions, nodes = parseInput input

        let startingNodes = nodes.Keys |> Seq.filter (_.EndsWith('A')) |> Array.ofSeq

        startingNodes
        |> Array.map (fun node -> walk instructions 0 nodes node 0)
        |> Array.map getPrimes
        |> getCommonPrimes
        |> List.map uint64
        |> List.reduce (*)
