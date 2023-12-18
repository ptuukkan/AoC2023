namespace AoC2023

open System

module Day16 =
    type Direction =
        | Up
        | Right
        | Down
        | Left

    let getSymbol (contraption: char array array) x y =
        match contraption |> Array.tryItem y with
        | Some line ->
            match line |> Array.tryItem x with
            | Some c -> Some c
            | None -> None
        | None -> None

    let getNext (direction: Direction) x y =
        match direction with
        | Up -> x, y - 1
        | Right -> x + 1, y
        | Down -> x, y + 1
        | Left -> x - 1, y

    let turn (mirror: char) (direction: Direction) x y =
        match mirror, direction with
        | '/', Up
        | '\\', Down -> Right, getNext Right x y
        | '\\', Up
        | '/', Down -> Left, getNext Left x y
        | '/', Right
        | '\\', Left -> Up, getNext Up x y
        | '\\', Right
        | '/', Left -> Down, getNext Down x y
        | _, _ -> raise (Exception($"unexpected mirror char {mirror} at {x},{y} going {direction}"))

    let isNewStep (trail: (Direction * (int * int)) list) direction x y =
        trail
        |> List.tryFind (fun (dir, (jx, jy)) -> dir = direction && jx = x && jy = y)
        |> Option.isNone

    let memoized (trails: (Direction * (int * int)) list list) direction x y =
        trails
        |> List.tryFind (fun trail -> trail |> List.contains (direction, (x, y)))
        |> Option.map (fun trail ->
            let index =
                trail |> List.findIndex (fun (d, (sx, sy)) -> d = direction && sx = x && sy = y)

            trail[index..])

    let energize (contraption: char array array) (trails: (Direction * (int * int)) list list) direction x y =
        let rec beam (direction: Direction) ((x, y): int * int) (trail: (Direction * (int * int)) list) =
            match memoized trails direction x y with
            | Some memoizedTrail ->
                Return(trail @ memoizedTrail)
            | None ->
                if isNewStep trail direction x y then
                    match direction, getSymbol contraption x y with
                    | _, Some '.'
                    | Up, Some '|'
                    | Down, Some '|'
                    | Right, Some '-'
                    | Left, Some '-' ->
                        let next = (direction, x, y) |||> getNext
                        let newTrail = trail @ [ (direction, (x, y)) ]
                        Suspend(fun () -> beam direction next newTrail)
                    | _, Some '-' ->
                        let nextRight = (Right, x, y) |||> getNext
                        let nextLeft = (Left, x, y) |||> getNext
                        let newTrail = trail @ [ (direction, (x, y)) ]

                        Suspend(fun () -> beam Right nextRight newTrail)
                        >>= Return
                        >>= (fun curr -> Suspend(fun () -> beam Left nextLeft curr))
                    | _, Some '|' ->
                        let nextUp = (Up, x, y) |||> getNext
                        let nextDown = (Down, x, y) |||> getNext
                        let newTrail = trail @ [ (direction, (x, y)) ]

                        Suspend(fun () -> beam Up nextUp newTrail)
                        >>= Return
                        >>= (fun curr -> Suspend(fun () -> beam Down nextDown curr))
                    | _, Some mirror ->
                        let nextDir, next = (direction, x, y) |||> turn mirror
                        let newTrail = trail @ [ (direction, (x, y)) ]

                        Suspend(fun () -> beam nextDir next newTrail)
                    | _, None -> Return trail
                else
                    Return trail

        beam direction (x, y) [] |> execute

    let part1 (input: string seq) =
        let contraption = input |> Array.ofSeq |> Array.map _.ToCharArray()

        energize contraption [] Right 0 0
        |> List.map snd
        |> List.distinct
        |> List.length

    let part2 (input: string seq) =
        let contraption = input |> Array.ofSeq |> Array.map _.ToCharArray()

        let length = contraption.Length

        let rights = [ 0..length ] |> List.map (fun i -> Right, 0, i)
        let downs = [ 0..length ] |> List.map (fun i -> Down, i, 0)
        let lefts = [ 0..length ] |> List.map (fun i -> Left, length - 1, i)
        let ups = [ 0..length ] |> List.map (fun i -> Up, i, length - 1)
        let entries = rights |> List.append downs |> List.append lefts |> List.append ups

        ([ [] ], entries)
        ||> List.fold (fun trails entry -> trails @ [ (entry |||> energize contraption trails) ])
        |> List.map (List.map snd)
        |> List.map List.distinct
        |> List.map List.length
        |> List.max
