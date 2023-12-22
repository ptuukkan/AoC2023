namespace AoC2023

open System.Collections.Generic
open FSharpx.Collections

module Day21 =

    let getReachableTiles (x: int) (y: int) (steps: int) (farm: char array array) =
        [| x, y - 1; x + 1, y; x, y + 1; x - 1, y |]
        |> Array.filter (fun (nx, ny) -> nx >= 0 && ny >= 0 && nx < farm.Length && ny < farm.Length)
        |> Array.filter (fun (nx, ny) -> farm[ny][nx] <> '#')
        |> Array.map (fun (nx, ny) -> steps, nx, ny)

    let getInfiniteTiles (x: int) (y: int) (fx: int) (fy: int) (steps: int) (farm: char array array) =
        [| x, y - 1; x + 1, y; x, y + 1; x - 1, y |]
        |> Array.map (fun (nx, ny) ->
            match nx, ny with
            | _ when nx < 0 -> farm.Length - 1, ny, fx - 1, fy
            | _ when nx >= farm.Length -> 0, ny, fx + 1, fy
            | _ when ny < 0 -> nx, farm.Length - 1, fx, fy - 1
            | _ when ny >= farm.Length -> x, 0, fx, fy + 1
            | _ -> nx, ny, fx, fy)
        |> Array.filter (fun (nx, ny, fx, fy) -> farm[ny][nx] <> '#')
        |> Array.map (fun (nx, ny, fx, fy) -> steps, nx, ny, fx, fy)

    let walk (x: int) (y: int) (maxSteps: int) (farm: char array array) =
        let visited = Dictionary<int * int, int>()

        let rec walk' (stack: IPriorityQueue<int * int * int>) =
            match stack |> PriorityQueue.tryPeek with
            | None -> ()
            | Some(ts, tx, ty) ->
                if ts = maxSteps then
                    ()
                else
                    let (hs, hx, hy), tail = stack |> PriorityQueue.pop

                    let new_stack =
                        farm
                        |> getReachableTiles hx hy (hs + 1)
                        |> Array.filter (fun (s, x, y) -> visited.ContainsKey(x, y) |> not)
                        |> Array.fold
                            (fun st (s, x, y) ->
                                visited.Add((x, y), s)
                                st |> PriorityQueue.insert (s, x, y))
                            tail

                    walk' new_stack

        let stack = PriorityQueue.empty<int * int * int> false

        farm
        |> getReachableTiles x y 1
        |> Array.fold (fun st tile -> st |> PriorityQueue.insert tile) stack
        |> walk'

        visited

    let findPattern (cycle: int) (patternLength: int) (history: (int * int) list) =
        if cycle < (patternLength * 4) then
            None
        else
            let magicNumbers =
                history
                |> List.map snd
                |> List.chunkBySize patternLength
                |> List.map (fun x -> x |> List.head)
                |> List.pairwise
                |> List.map (fun (a, b) -> b - a)
                |> List.pairwise
                |> List.map (fun (a, b) -> b - a)
                |> List.rev
                |> List.take 2

            let magic = magicNumbers |> List.head

            if magicNumbers |> List.forall (fun x -> x = magic) then
                Some(magic, history)
            else
                None

    let infiniteWalk (x: int) (y: int) (patternLength: int) (farm: char array array) =
        let visited = Dictionary<int * int * int * int, int>()

        let rec infiniteWalk'
            (stack: IPriorityQueue<int * int * int * int * int>)
            (prev_cycle: int)
            (prev_history: (int * int) list)
            =
            let mutable cycle = prev_cycle
            let mutable history = prev_history
            let mutable pattern = None

            match stack |> PriorityQueue.tryPop with
            | None -> failwith "todo"
            | Some((hs, hx, hy, fx, fy), tail) ->
                if cycle <> hs then
                    cycle <- hs
                    let length = visited |> Seq.filter (fun x -> x.Value % 2 = cycle % 2) |> Seq.length
                    history <- history @ [ (cycle, length) ]
                    pattern <- findPattern cycle patternLength history

                match pattern with
                | None ->
                    let new_stack =
                        farm
                        |> getInfiniteTiles hx hy fx fy (hs + 1)
                        |> Array.filter (fun (s, x, y, fx, fy) -> visited.ContainsKey(x, y, fx, fy) |> not)
                        |> Array.fold
                            (fun st (s, x, y, fx, fy) ->
                                visited.Add((x, y, fx, fy), s)
                                st |> PriorityQueue.insert (s, x, y, fx, fy))
                            tail

                    infiniteWalk' new_stack cycle history
                | Some pattern -> pattern

        let stack = PriorityQueue.empty<int * int * int * int * int> false

        let new_stack =
            farm
            |> getInfiniteTiles x y 0 0 1
            |> Array.fold (fun st tile -> st |> PriorityQueue.insert tile) stack

        infiniteWalk' new_stack 1 []

    let part1 (input: string seq) =
        let farm = input |> Array.ofSeq |> Array.map _.ToCharArray()

        let start_y =
            farm
            |> Array.findIndex (fun x -> x |> Array.tryFind (fun b -> b = 'S') |> Option.isSome)

        let start_x = farm[start_y] |> Array.findIndex (fun x -> x = 'S')

        farm
        |> walk start_x start_y 64
        |> Seq.filter (fun kv -> kv.Value % 2 = 0)
        |> Seq.length

    let part2 (input: string seq) =
        let farm = input |> Array.ofSeq |> Array.map _.ToCharArray()
        let target = 26501365

        let start_y =
            farm
            |> Array.findIndex (fun x -> x |> Array.tryFind (fun b -> b = 'S') |> Option.isSome)

        let start_x = farm[start_y] |> Array.findIndex (fun x -> x = 'S')

        let magic, history = farm |> infiniteWalk start_x start_y farm.Length

        let cycle, _a = List.last history
        let start = ((target - cycle - farm.Length) % farm.Length) + (cycle - farm.Length)
        let _c, start_length = history[start - 2]
        let n = (target - start) / farm.Length |> double

        let diff =
            let _c, prev_length = history[start - farm.Length - 2]
            start_length - prev_length |> double

        double start_length + (diff * n) + (n / 2.0) * (1.0 + n) * double magic
        |> uint64
