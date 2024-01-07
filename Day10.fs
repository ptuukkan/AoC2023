namespace AoC2023

module Day10 =
    type Direction =
        | North
        | East
        | South
        | West

    let pipes =
        [ '|', [ North; South ]
          '-', [ East; West ]
          'L', [ North; East ]
          'J', [ North; West ]
          '7', [ South; West ]
          'F', [ South; East ]
          'S', [ North; East; South; West ] ]

    let canMove (dir: Direction) (from: char) =
        pipes |> List.find (fun x -> from = fst x) |> snd |> List.contains dir

    let possiblePipes (dir: Direction) =
        pipes
        |> List.filter (fun (pipe, dirs) -> dirs |> List.contains dir)
        |> List.map fst

    let north (y: int) (x: int) (pipe: char) =
        if canMove North pipe then
            Some(y - 1, x, possiblePipes South)
        else
            None

    let east (y: int) (x: int) (pipe: char) =
        if canMove East pipe then
            Some(y, x + 1, possiblePipes West)
        else
            None

    let south (y: int) (x: int) (pipe: char) =
        if canMove South pipe then
            Some(y + 1, x, possiblePipes North)
        else
            None

    let west (y: int) (x: int) (pipe: char) =
        if canMove West pipe then
            Some(y, x - 1, possiblePipes East)
        else
            None

    let findStartingPos (diagram: char array array) =
        let y =
            diagram
            |> Array.findIndex (fun a -> a |> Array.tryFind (fun b -> b = 'S') |> Option.isSome)

        let x = diagram[y] |> Array.findIndex (fun x -> x = 'S')
        y, x

    let findConnectingPipe (y: int) (x: int) (py: int) (px: int) (diagram: char array array) =
        let currentPipe = diagram[y][x]

        [ north y x currentPipe
          east y x currentPipe
          south y x currentPipe
          west y x currentPipe ]
        |> List.choose id
        |> List.filter (fun (ay, ax, _pb) -> (ay = py && ax = px) |> not)
        |> List.filter (fun (ay, _ax, _pb) -> diagram |> Array.tryItem ay |> Option.isSome)
        |> List.filter (fun (ay, ax, _pb) -> diagram[ay] |> Array.tryItem ax |> Option.isSome)
        |> List.find (fun (ay, ax, pb) -> pb |> List.contains (diagram[ay][ax]))
        |> (fun (ay, ax, _pb) -> ay, ax)

    let countSteps (diagram: char array array) (startY: int) (startX: int) =
        let rec crawl (diagram: char array array) (steps: int) (py: int) (px: int) (y: int) (x: int) =
            if diagram[y][x] = 'S' && steps <> 0 then
                steps
            else
                let ny, nx = diagram |> findConnectingPipe y x py px
                crawl diagram (steps + 1) y x ny nx

        crawl diagram 0 startY startX startY startX

    let getBorder (diagram: char array array) (startY: int) (startX: int) =
        let rec crawl
            (diagram: char array array)
            (border: (int * int * char) list)
            (py: int)
            (px: int)
            (y: int)
            (x: int)
            =
            if diagram[y][x] = 'S' && border |> List.isEmpty |> not then
                border
            else
                let ny, nx = diagram |> findConnectingPipe y x py px
                crawl diagram (border @ [ (y, x, diagram[y][x]) ]) y x ny nx

        crawl diagram [] startY startX startY startX

    let borderCrossings (border: (int * int * char) list) (y: int) (x: int) (direction: Direction) =
        match direction with
        | North ->
            border
            |> List.filter (fun (by, bx, _bc) -> bx = x && by < y)
            |> List.sortByDescending (fun (a, _b, _c) -> a)
        | East ->
            border
            |> List.filter (fun (by, bx, _bc) -> bx > x && by = y)
            |> List.sortBy (fun (_a, b, _c) -> b)
        | South ->
            border
            |> List.filter (fun (by, bx, _bc) -> bx = x && by > y)
            |> List.sortBy (fun (a, _b, _c) -> a)
        | West ->
            border
            |> List.filter (fun (by, bx, _bc) -> bx < x && by = y)
            |> List.sortByDescending (fun (_a, b, _c) -> b)
        |> List.map (fun (_by, _bx, bc) -> bc)
        |> List.fold
            (fun (crossings, corner) borderChar ->
                match borderChar, corner, direction with
                | '|', _, East
                | '|', _, West
                | '-', _, North
                | '-', _, South -> (crossings + 1, corner)
                | 'L', _, North
                | 'J', _, North
                | 'L', _, East
                | 'F', _, East
                | 'F', _, South
                | '7', _, South
                | 'J', _, West
                | '7', _, West -> (crossings, Some borderChar)
                | 'F', Some 'L', North
                | '7', Some 'J', North
                | 'L', Some 'F', South
                | 'J', Some '7', South
                | 'J', Some 'L', East
                | '7', Some 'F', East
                | 'L', Some 'J', West
                | 'F', Some '7', West -> (crossings, None)
                | '7', Some 'L', North
                | 'F', Some 'J', North
                | 'J', Some 'F', South
                | 'L', Some '7', South
                | '7', Some 'L', East
                | 'J', Some 'F', East
                | 'F', Some 'J', West
                | 'L', Some '7', West -> (crossings + 1, None)
                | _, _, _ -> (crossings, corner))
            (0, None)
        |> fst

    let part1 (input: string seq) =
        let diagram = input |> Array.ofSeq |> Array.map (_.ToCharArray())
        diagram |> findStartingPos ||> countSteps diagram |> (fun steps -> steps / 2)

    let part2 (input: string seq) =
        let diagram = input |> Array.ofSeq |> Array.map (_.ToCharArray())
        let border = diagram |> findStartingPos ||> getBorder diagram

        diagram
        |> Array.mapi (fun y line ->
            line
            |> Array.mapi (fun x c ->
                if border |> List.tryFind (fun (by, bx, c) -> by = y && bx = x) |> Option.isSome then
                    c
                else
                    match
                        [ borderCrossings border y x North
                          borderCrossings border y x East
                          borderCrossings border y x South
                          borderCrossings border y x West ]
                        |> List.min
                        |> (fun x -> if x = 0 then 0 else x % 2)
                    with
                    | 1 -> 'I'
                    | _ -> 'O')
            |> Array.filter (fun c -> c = 'I')
            |> Array.length)
        |> Array.sum
