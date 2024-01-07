namespace AoC2023

open System
open System.Collections.Generic
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

    let memoize f =
        let dict = Dictionary<_, _>()

        fun src dst curr visited ->
            let exist, value = dict.TryGetValue((curr, dst))

            match exist with
            | true ->
                // printfn "memoized!"
                value
            | _ ->
                let value = f src dst curr visited
                dict.Add((curr, dst), value)
                value

    let traverse (graph: Map<string, string list>) (combinations: string list list) =
        let visited = HashSet<string * string * string>()
        let memo = Dictionary<_, _>()

        let concatPathAndSave (path: string list option) (curr: string) (dst: string) =
            path
            |> Option.bind (fun p ->
                let newPath = curr :: p
                let exists, value = memo.TryGetValue((curr, dst))

                if exists && (List.length newPath) < (List.length value) then
                    memo.Remove((curr, dst)) |> ignore
                    memo.Add((curr, dst), newPath)
                elif exists |> not then
                    memo.Add((curr, dst), newPath)

                Some(newPath))
            |> Return

        let rec traverse' (src: string) (dst: string) (curr: string) (trail: string list) =
            if curr = dst then
                Return(Some([ curr ]))
            else
                // visited.Add(src, dst, curr) |> ignore

                let exist, value = memo.TryGetValue((curr, dst))
                if exist then
                    printfn "memoized!"
                    Return(Some(value))
                else

                    let connections =
                        graph[curr] |> List.filter (fun x -> trail |> List.contains x |> not)

                    match connections with
                    | [] -> Return(None)
                    | [ next ] ->
                        Suspend(fun () -> traverse' src dst next (curr :: trail))
                        >>= (fun path -> concatPathAndSave path curr dst)
                    | _ ->
                        connections
                        |> List.map (fun next -> Suspend(fun () -> traverse' src dst next (curr :: trail)))
                        |> List.reduce (fun acc e ->
                            acc
                            >>= (fun a ->
                                e
                                >>= (fun b ->
                                    match a, b with
                                    | Some aa, Some bb -> if aa.Length < bb.Length then Return a else Return b
                                    | Some _, _ -> Return a
                                    | _, Some _ -> Return b
                                    | _ -> Return None)))
                        >>= (fun path -> concatPathAndSave path curr dst)
        // let head = graph.Keys |> Seq.head
        // graph.Keys
        // |> Seq.filter (fun x -> x <> head)
        // |> Seq.map (fun dst -> traverse' head dst head [] |> execute)
        // |> List.ofSeq
        // |> List.skip 1
        // |> List.head
        // |> (fun nodes -> nodes, traverse' nodes[0] nodes[1] nodes[0] [] |> execute)
        combinations
        |> List.map (fun nodes -> traverse' nodes[0] nodes[1] nodes[0] [] |> execute)
        |> List.choose id

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
                graph[current]
                |> List.filter (fun x -> visited |> List.contains x |> not)
                |> List.fold (fun new_stack next_node -> new_stack |> Queue.conj next_node) tail
                |> countNodes' (current :: visited)

        Queue.empty<string> |> Queue.conj (graph.Keys |> Seq.head) |> countNodes' []

    let part1 (input: string seq) =
        let graph =
            input |> Seq.map split |> Seq.fold createMap Map.empty<string, string list>

        // let wiresToDisconnect =
        graph.Keys
        |> List.ofSeq
        |> combinations 2
        |> traverse graph
        // |> connectionPairs
        // |> uniqueOccurrences
        // |> List.take 3
        |> List.iter (printfn "%0A")
// |> (printfn "%0A")
//
// printfn "%0A" wiresToDisconnect
// let group1 = wiresToDisconnect |> List.fold disconnect graph |> countNodes
// graph.Keys |> Seq.length |> (fun total -> group1 * (total - group1))
