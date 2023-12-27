namespace AoC2023

module Day22 =
    type Point =
        { X: int
          Y: int
          Z: int
          Label: string }

    let parse (index: int) (line: string) =
        let split = line.Split('~')
        let a = split[0].Split(',') |> Array.map int
        let b = split[1].Split(',') |> Array.map int

        { X = a[0]
          Y = a[1]
          Z = a[2]
          Label = string index },
        { X = b[0]
          Y = b[1]
          Z = b[2]
          Label = string index }

    let intersects (a_lines: Set<int>) (a_columns: Set<int>) (b_lines: int list) (b_columns: int list) =
        b_lines |> Set.ofList |> Set.intersect a_lines |> Set.isEmpty |> not
        && b_columns |> Set.ofList |> Set.intersect a_columns |> Set.isEmpty |> not

    let bricksBelow ((ba, bb): Point * Point) (bricks: (Point * Point) array) =
        let brickLines = [ ba.X .. bb.X ] |> Set.ofList
        let brickColumns = [ ba.Y .. bb.Y ] |> Set.ofList

        bricks
        |> Array.filter (fun (a, b) -> intersects brickLines brickColumns [ a.X .. b.X ] [ a.Y .. b.Y ])

    let supportsBricks (bricks: (Point * Point) array) ((a, b): Point * Point) =
        let maxZ = max a.Z b.Z
        let brickLines = [ a.X .. b.X ] |> Set.ofList
        let brickColumns = [ a.Y .. b.Y ] |> Set.ofList

        bricks
        |> Array.filter (fun (a2, b2) -> min a2.Z b2.Z = maxZ + 1)
        |> Array.filter (fun (a2, b2) -> intersects brickLines brickColumns [ a2.X .. b2.X ] [ a2.Y .. b2.Y ])

    let supportedBricks (bricks: (Point * Point) array) ((a, b): Point * Point) =
        let minZ = min a.Z b.Z
        let brickLines = [ a.X .. b.X ] |> Set.ofList
        let brickColumns = [ a.Y .. b.Y ] |> Set.ofList

        bricks
        |> Array.filter (fun (a2, b2) -> max a2.Z b2.Z + 1 = minZ)
        |> Array.filter (fun (a2, b2) -> intersects brickLines brickColumns [ a2.X .. b2.X ] [ a2.Y .. b2.Y ])

    let highestZ (bricks: (Point * Point) array) =
        match bricks with
        | [||] -> 1
        | other ->
            other
            |> Array.maxBy (fun (a, b) -> max a.Z b.Z)
            |> (fun (a, b) -> max a.Z b.Z)
            |> (+) 1

    let fallBrick (bricks: (Point * Point) array) ((a, b): Point * Point) (z: int) =
        [| { a with Z = a.Z - (a.Z - z) }, { b with Z = b.Z - (a.Z - z) } |]
        |> Array.append bricks

    let part1 (input: string seq) =
        let bricks = input |> Array.ofSeq |> Array.mapi parse

        let fallenBricks =
            bricks
            |> Array.sortBy (fun (a, b) -> min a.Z b.Z)
            |> Array.fold
                (fun fallenBricks currentBrick ->
                    fallenBricks
                    |> bricksBelow currentBrick
                    |> highestZ
                    |> fallBrick fallenBricks currentBrick)
                [||]

        let supportBricks =
            fallenBricks
            |> Array.map (fun brick -> (fst brick).Label, supportsBricks fallenBricks brick)

        supportBricks
        |> Array.filter (fun (label, supports) ->
            match supports with
            | [||] -> true
            | supported ->
                supported
                |> Array.forall (fun x1 ->
                    supportBricks
                    |> Array.tryFind (fun (l, b) ->
                        l <> label
                        && b |> Array.tryFind (fun x2 -> (fst x1).Label = (fst x2).Label) |> Option.isSome)
                    |> Option.isSome))
        |> Array.length

    let part2 (input: string seq) =
        let bricks = input |> Array.ofSeq |> Array.mapi parse

        let fallenBricks =
            bricks
            |> Array.sortBy (fun (a, b) -> min a.Z b.Z)
            |> Array.fold
                (fun fallenBricks currentBrick ->
                    fallenBricks
                    |> bricksBelow currentBrick
                    |> highestZ
                    |> fallBrick fallenBricks currentBrick)
                [||]

        let supportedBricks =
            fallenBricks
            |> Array.map (fun brick -> (fst brick).Label, supportedBricks fallenBricks brick)

        supportedBricks
        |> Array.mapi (fun i (label, b) ->
            label,
            ([ label ], supportedBricks[i..])
            ||> Array.fold (fun disintegrated (l, supported) ->
                match supported with
                | [||] -> disintegrated
                | some ->
                    match
                        some
                        |> Array.filter (fun b -> disintegrated |> List.contains (fst b).Label |> not)
                    with
                    | [||] -> l :: disintegrated
                    | _ -> disintegrated))
        |> Array.map (fun (l, bricks) -> bricks |> List.filter (fun a -> a <> l) |> List.distinct)
        |> Array.map List.length
        |> Array.sum
