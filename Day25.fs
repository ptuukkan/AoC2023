namespace AoC2023

open System
open FSharpx.Collections

module Day25 =

    let rec combinations n l =
        match n, l with
        | 0, _ -> [ [] ]
        | _, [] -> []
        | k, x :: xs -> List.map ((@) [ x ]) (combinations (k - 1) xs) @ combinations k xs

    let addConnection src dst (map: Map<string, string list>) =
        match map |> Map.tryFind src with
        | Some connections ->
            if connections |> List.contains dst then
                map
            else
                map |> Map.add src (dst :: connections)
        | None -> map |> Map.add src [ dst ]

    let addConnections (map: Map<string, string list>) ((src, dst): string * string) =
        map |> addConnection src dst |> addConnection dst src

    let createMap (map: Map<string, string list>) ((src, dst): string * string array) =
        [| src |] |> Array.allPairs dst |> Array.fold addConnections map

    let split (str: string) =
        let s = str.Split(':', StringSplitOptions.TrimEntries)
        s[0], s[1].Split(' ', StringSplitOptions.TrimEntries)

    let traverse (graph: Map<string, string list>) (src: string) (dst: string) =
        let rec traverse' (visited: string list) (queue: IPriorityQueue<int * string * string list>) =
            match queue |> PriorityQueue.tryPop with
            | None -> failwith "todo"
            | Some((steps, curr, trail), tail) ->
                if curr = dst then
                    trail
                else
                    let connections =
                        graph[curr] |> List.filter (fun x -> visited |> List.contains x |> not)

                    let new_queue =
                        connections
                        |> List.fold (fun q next -> q |> PriorityQueue.insert (steps + 1, next, curr :: trail)) tail

                    traverse' (curr :: visited) new_queue

        PriorityQueue.empty false |> PriorityQueue.insert (0, src, []) |> traverse' []

    let connectionPairs (paths: string list list) =
        paths
        |> List.map List.pairwise
        |> List.concat
        |> List.map (fun (a, b) -> [ a; b ] |> List.sort)

    let uniqueOccurrences (connectionPairs: string list list) =
        connectionPairs
        |> List.groupBy id
        |> List.map (fun (pair, occurrences) -> pair, occurrences |> List.length)
        |> List.sortByDescending snd
        |> List.fold
            (fun connections (connection, count) ->
                if
                    connection
                    |> List.filter (fun x -> connections |> List.concat |> List.contains x |> not)
                    |> List.length = 2
                then
                    connections @ [ connection ]
                else
                    connections)
            []

    let disconnect (graph: Map<string, string list>) (wire: string list) =
        graph
        |> Map.change wire[0] (fun x ->
            match x with
            | None -> failwith "todo"
            | Some connections -> connections |> List.filter (fun c -> c <> wire[1]) |> Some)
        |> Map.change wire[1] (fun x ->
            match x with
            | None -> failwith "todo"
            | Some connections -> connections |> List.filter (fun c -> c <> wire[0]) |> Some)

    let countNodes (graph: Map<string, string list>) =
        let rec countNodes' (visited: string list) (stack: Queue<string>) =
            match stack |> Queue.tryUncons with
            | None -> visited |> List.distinct |> List.length
            | Some(current, tail) ->
                let q =
                    graph[current]
                    |> List.filter (fun x -> visited |> List.contains x |> not)
                    |> List.fold (fun new_stack next_node -> new_stack |> Queue.conj next_node) tail

                countNodes' (current :: visited) q

        Queue.empty<string> |> Queue.conj (graph.Keys |> Seq.head) |> countNodes' []

    let part1 (input: string seq) =
        let graph =
            input |> Seq.map split |> Seq.fold createMap Map.empty<string, string list>

        let rnd = Random()
        let pairs = graph.Keys |> List.ofSeq |> combinations 2

        let randomPairs =
            [ for _ in 1..100 -> rnd.Next(0, pairs.Length) ] |> List.map (fun i -> pairs[i])

        let wiresToDisconnect =
            randomPairs
            |> List.map (fun nodes -> traverse graph nodes[0] nodes[1])
            |> connectionPairs
            |> uniqueOccurrences
            |> List.take 3

        let group1 = wiresToDisconnect |> List.fold disconnect graph |> countNodes
        graph.Keys |> Seq.length |> (fun total -> group1 * (total - group1))
