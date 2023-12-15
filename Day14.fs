namespace AoC2023

module Day14 =
    let tilt (line: char array) =
        let rec tiltInner startIndex tiltedLine =
            match line[startIndex..] |> Array.tryFindIndex (fun x -> x = '#') with
            | Some cube ->
                let rounded =
                    line[startIndex .. (startIndex + cube)] |> Array.filter (fun x -> x = 'O')

                let slice =
                    [| 0 .. (cube - rounded.Length - 1) |]
                    |> Array.map (fun x -> '.')
                    |> Array.append rounded
                    |> Array.append tiltedLine

                tiltInner (startIndex + cube + 1) (Array.append slice [| '#' |])
            | None ->
                let rounded = line[startIndex..] |> Array.filter (fun x -> x = 'O')

                let slice =
                    [| 0 .. (line.Length - startIndex - rounded.Length - 1) |]
                    |> Array.map (fun x -> '.')
                    |> Array.append rounded
                    |> Array.append tiltedLine

                slice

        tiltInner 0 [||]

    let part1 (input: string seq) =
        let platform =
            input
            |> Array.ofSeq
            |> Array.map _.ToCharArray()
            |> Array.transpose
            |> Array.map tilt
            |> Array.transpose

        platform
        |> Array.mapi (fun i line ->
            line
            |> Array.map (fun c ->
                match c with
                | 'O' -> platform.Length - i
                | _ -> 0)
            |> Array.sum)
        |> Array.sum

    let validatePattern (pattern: (int * int) array) (prevPattern: (int * int) array) =
        let valid =
            pattern
            |> Array.mapi (fun i (x, l) -> l = (prevPattern[i] |> snd))
            |> Array.forall id

        if valid then Some(pattern |> Array.tail) else None

    let findPattern (loads: (int * int) array) =
        match loads |> Array.tryLast with
        | Some(iter, load) ->
            match loads |> Array.tryFindIndexBack (fun (i, l) -> i <> iter && load = l) with
            | Some(index) when loads.Length > ((loads.Length - index) * 2) ->
                validatePattern loads[index..] loads[index - (loads.Length - index) + 1 .. index]
            | _ -> None
        | None -> None


    let rec cycle (iter: int) (loads: (int * int) array) (platform: char array array) =
        match findPattern loads with
        | Some pattern -> pattern
        | None ->
            let newPlatform =
                platform
                |> Array.transpose
                |> Array.map tilt // north
                |> Array.transpose
                |> Array.map tilt // west
                |> Array.transpose
                |> Array.map Array.rev
                |> Array.map tilt // south
                |> Array.map Array.rev
                |> Array.transpose
                |> Array.map Array.rev
                |> Array.map tilt //east
                |> Array.map Array.rev

            let load =
                newPlatform
                |> Array.mapi (fun i line ->
                    line
                    |> Array.map (fun c ->
                        match c with
                        | 'O' -> newPlatform.Length - i
                        | _ -> 0)
                    |> Array.sum)
                |> Array.sum

            cycle (iter + 1) (Array.append loads [| iter, load |]) newPlatform

    let part2 (input: string seq) =
        let loadCycles = input |> Array.ofSeq |> Array.map _.ToCharArray() |> cycle 1 [||]

        let iter = loadCycles |> Array.head |> fst
        loadCycles[(1000000000 - iter) % loadCycles.Length] |> snd
