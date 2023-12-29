namespace AoC2023

open System

module Day23 =
    type Node =
        { Id: int * int
          Connections: (int * (int * int)) array }

    let getAdjacentTiles (trails: char array array) (x: int) (y: int) =
        [| x, y - 1; x + 1, y; x, y + 1; x - 1, y |]
        |> Array.filter (fun (x, y) -> x >= 0 && y >= 0 && x < trails.Length && y < trails.Length)
        |> Array.map (fun (x, y) -> x, y, trails[y][x])
        |> Array.filter (fun (x, y, c) -> c <> '#')
        |> Array.map (fun (x, y, c) -> x, y)

    let createNodes (trails: char array array) =
        trails
        |> Array.map Array.indexed
        |> Array.indexed
        |> Array.map (fun (y, line) ->
            y,
            line
            |> Array.filter (fun (x, c) -> c <> '#')
            |> Array.filter (fun (x, c) -> getAdjacentTiles trails x y |> Array.length |> (fun x -> x > 2 || x < 2)))
        |> Array.filter (fun (y, nodes) -> nodes.Length > 0)
        |> Array.map (fun (y, nodes) -> nodes |> Array.map (fun (x, node) -> x, y, node))
        |> Array.concat
        |> Array.map (fun (x, y, c) -> x, y)

    let connectNodes (trails: char array array) (slippery: bool) (node: int * int) =
        let rec followTrail (x: int) (y: int) (dx: int) (dy: int) (steps: int) =
            match trails[y][x] with
            | '>' when dx = -1 && slippery -> None
            | 'v' when dy = -1 && slippery -> None
            | '<' when dx = 1 && slippery -> None
            | '^' when dy = 1 && slippery -> None // Can't go up the slope
            | _ ->
                let availablePaths =
                    (x, y)
                    ||> getAdjacentTiles trails
                    |> Array.filter (fun (x2, y2) -> (x2, y2) <> (x - dx, y - dy)) // Eliminate previous tile

                if availablePaths.Length > 1 || availablePaths.Length = 0 then // We are there
                    Some(steps, (x, y))
                else
                    let nx, ny = Array.head availablePaths
                    followTrail nx ny (nx - x) (ny - y) (steps + 1)

        let connections =
            node
            ||> getAdjacentTiles trails
            |> Array.map (fun (x, y) -> followTrail x y (x - fst node) (y - snd node) 1)
            |> Array.choose id

        node, connections

    let createGraph (slippery: bool) (trails: char array array) =
        trails |> createNodes |> Array.map (connectNodes trails slippery)

    let findLongestPath (graph: ((int * int) * (int * (int * int)) array) array) =
        let startNode = graph |> Array.head |> fst
        let endNode = graph |> Array.last |> fst
        let graphMap = graph |> Map.ofArray

        let rec findLongestPath' (currentNode: int * int) (trail: (int * int) list) =
            if (currentNode = endNode) then
                Some(0)
            else
                let paths =
                    graphMap[currentNode]
                    |> Array.filter (fun (length, dst) -> trail |> List.contains dst |> not)

                match paths with
                | [||] -> None
                | _ ->
                    paths
                    |> Array.map (fun (length, dst) ->
                        findLongestPath' dst (currentNode :: trail)
                        |> Option.bind (fun steps -> Some(steps + length)))
                    |> Array.choose id
                    |> (fun x ->
                        match x with
                        | [||] -> None
                        | _ -> x |> Array.max |> Some)

        findLongestPath' startNode []

    let part1 (input: string seq) =
        input
        |> Array.ofSeq
        |> Array.map _.ToCharArray()
        |> createGraph true
        |> findLongestPath

    let part2 (input: string seq) =
        input
        |> Array.ofSeq
        |> Array.map _.ToCharArray()
        |> createGraph false
        |> findLongestPath
