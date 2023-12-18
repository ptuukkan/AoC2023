namespace AoC2023

open System.Collections.Generic

module Day17 =

    let visited = HashSet<int * int * int * int * int>()

    let add
        (queue: PriorityQueue<int * int * int * int * int * int, int>)
        heatLoss
        x
        y
        dx
        dy
        steps
        (city: int array array)
        =
        let newX = x + dx
        let newY = y + dy

        if newX >= 0 && newY >= 0 && newX < city.Length && newY < city.Length then
            let newHeatLoss = heatLoss + city[newY][newX]
            queue.Enqueue((newHeatLoss, newX, newY, dx, dy, steps), newHeatLoss)

    let add2
        (queue: PriorityQueue<int * int * int * int * int * int, int>)
        heatLoss
        x
        y
        dx
        dy
        steps
        (city: int array array)
        =
        let newX = x + dx
        let newY = y + dy
        let canTurnX = newX + dx + dx + dx
        let canTurnY = newY + dy + dy + dy

        if
            canTurnX >= 0
            && canTurnY >= 0
            && canTurnX < city.Length
            && canTurnY < city.Length
        then
            let newHeatLoss = heatLoss + city[newY][newX]
            queue.Enqueue((newHeatLoss, newX, newY, dx, dy, steps), newHeatLoss)

    let findPath (city: int array array) =
        let queue = PriorityQueue<int * int * int * int * int * int, int>()
        let mutable run = true
        let length = city.Length
        let mutable result = 0

        queue.Enqueue((0, 0, 0, 0, 0, 0), 0)

        while run do
            let heatLoss, x, y, dx, dy, steps = queue.Dequeue()

            if x = length - 1 && y = length - 1 then
                run <- false
                result <- heatLoss
            else if visited.Contains(x, y, dx, dy, steps) |> not then
                visited.Add(x, y, dx, dy, steps) |> ignore

                [ 0, 1; 1, 0; 0, -1; -1, 0 ]
                |> List.filter (fun (nx, ny) -> (nx, ny) <> (dx, dy) && (nx, ny) <> (-dx, -dy))
                |> List.iter (fun (nx, ny) -> add queue heatLoss x y nx ny 1 city)

                if steps < 3 && (dx, dy) <> (0, 0) then
                    add queue heatLoss x y dx dy (steps + 1) city

        result

    let ultraCrucible (city: int array array) =
        let queue = PriorityQueue<int * int * int * int * int * int, int>()
        let mutable run = true
        let length = city.Length
        let mutable result = 0

        queue.Enqueue((0, 0, 0, 0, 0, 0), 0)

        while run do
            let heatLoss, x, y, dx, dy, steps = queue.Dequeue()

            if x = length - 1 && y = length - 1 then
                run <- false
                result <- heatLoss
            else if visited.Contains(x, y, dx, dy, steps) |> not then
                visited.Add(x, y, dx, dy, steps) |> ignore

                // Turn
                if steps > 3 || (dx, dy) = (0, 0) then
                    [ 0, 1; 1, 0; 0, -1; -1, 0 ]
                    |> List.filter (fun (nx, ny) -> (nx, ny) <> (dx, dy) && (nx, ny) <> (-dx, -dy))
                    |> List.iter (fun (nx, ny) -> add2 queue heatLoss x y nx ny 1 city)

                // Straight
                if steps < 10 && (dx, dy) <> (0, 0) then
                    add queue heatLoss x y dx dy (steps + 1) city

        result

    let part1 (input: string seq) =
        input
        |> Array.ofSeq
        |> Array.map (_.ToCharArray() >> Array.map (string >> int))
        |> findPath

    let part2 (input: string seq) =
        input
        |> Array.ofSeq
        |> Array.map (_.ToCharArray() >> Array.map (string >> int))
        |> ultraCrucible
