namespace AoC2023

module Day11 =
    let connectGalaxies ((y1, x1), (y2, x2)) =
        let dx = abs (x2 - x1)
        let dy = abs (y2 - y1)
        let mutable x, y = x1, y1
        let x_inc = if x2 > x1 then 1 else -1
        let y_inc = if y2 > y1 then 1 else -1
        let mutable error = dx - dy
        let mutable y_error = 0
        let mutable x_error = 0
        let mutable run = true

        seq {
            while run do
                y_error <- error * 2 - (-dy)
                x_error <- dx - error * 2

                if y_error > x_error then
                    error <- error - dy
                    x <- x + x_inc
                else
                    error <- error + dx
                    y <- y + y_inc

                yield (y, x)

                if x = x2 && y = y2 then
                    run <- false
        }

    let constructUniverse (input: string seq) =
        input
        |> Array.ofSeq
        |> Array.map (_.ToCharArray() >> Array.map string)
        |> Array.fold
            (fun (arr, g) y ->
                let newLine, galaxies =
                    (([||], g), y)
                    ||> Array.fold (fun (line, g) x ->
                        match x with
                        | "#" -> ([| string g |] |> Array.append line, g + 1)
                        | _ -> ([| x |] |> Array.append line, g))

                ([| newLine |] |> Array.append arr, galaxies))
            ([||], 1)
        |> fst

    let expandUniverse (factor: int) (universe: string array array) =
        let expand (slice: string array) =
            if slice |> Array.forall (fun x -> x = ".") then
                slice |> Array.replicate factor
            else
                [| slice |]

        universe
        |> (Array.map expand >> Array.concat >> Array.transpose)
        |> (Array.map expand >> Array.concat >> Array.transpose)

    let findGalaxies (universe: string array array) =
        universe
        |> Array.mapi (fun y line ->
            line
            |> Array.indexed
            |> Array.filter (fun (_x, c) -> c <> ".")
            |> Array.map (fun (x, c) -> (y, x)))
        |> Array.concat

    let getGalaxyPairs (galaxies: (int * int) array) =
        galaxies
        |> Array.allPairs galaxies
        |> Array.filter (fun (a, b) -> a <> b)
        |> Array.distinctBy (fun ((y1, x1), (y2, x2)) ->
            if y1 < y2 then ((y1, x1), (y2, x2))
            elif y1 = y2 && x1 < x2 then ((y1, x1), (y2, x2))
            else ((y2, x2), (y1, x1)))

    let part1 (input: string seq) =
        input
        |> constructUniverse
        |> expandUniverse 2
        |> findGalaxies
        |> getGalaxyPairs
        |> Array.map (connectGalaxies >> Seq.length)
        |> Array.sum
        
    let part2 (input: string seq) =
        let shrank = 
            input
            |> constructUniverse
            |> expandUniverse 0
            |> findGalaxies
            |> getGalaxyPairs
            |> Array.map (connectGalaxies >> Seq.length >> uint64)
            
        let expanded =
            input
            |> constructUniverse
            |> expandUniverse 10
            |> findGalaxies
            |> getGalaxyPairs
            |> Array.map (connectGalaxies >> Seq.length >> uint64)
            
        shrank
        |> Array.zip expanded
        |> Array.map (fun (e, s) -> (e - s) * 100000UL + s)
        |> Array.sum
            