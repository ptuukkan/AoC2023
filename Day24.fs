namespace AoC2023

open System

module Day24 =
    let rec combinations n l =
        match n, l with
        | 0, _ -> [ [] ]
        | _, [] -> []
        | k, x :: xs -> List.map ((@) [ x ]) (combinations (k - 1) xs) @ combinations k xs

    let strToTuple (str: string) f =
        let split = str.Split(',', StringSplitOptions.TrimEntries) |> Array.map f
        split[0], split[1], split[2]

    let ignoreZ ((px, py, pz), (vx, vy, vz)) = (px, py), (vx, vy)

    let calculateIntersection (lines: ((double * double) * (double * double)) list) =
        let (x1, y1), (vx1, vy1) = lines[0]
        let (x3, y3), (vx2, vy2) = lines[1]
        let det = vx1 * vy2 - vy1 * vx2
        let t = ((x1 - x3) * -vy2 - (y1 - y3) * -vx2) / det
        let u = ((x1 - x3) * -vy1 - (y1 - y3) * -vx1) / det
        let px = x1 + t * vx1
        let py = y1 + t * vy1

        px, py, t, u

    let part1 (input: string seq) =
        input
        |> List.ofSeq
        |> List.map (fun x ->
            let split = x.Split('@', StringSplitOptions.TrimEntries)
            strToTuple split[0] double, strToTuple split[1] double)
        |> List.map ignoreZ
        |> combinations 2
        |> List.map calculateIntersection
        |> List.filter (fun (px, py, t, u) ->
            t >= 0.0
            && u >= 0.0
            && px >= 200000000000000.0
            && py >= 200000000000000.0
            && px <= 400000000000000.0
            && py <= 400000000000000.0)
        |> List.length

    let subtract (a1, a2, a3) (b1, b2, b3) = a1 - b1, a2 - b2, a3 - b3

    let diff (pa, va) (pb, vb) = (subtract pb pa), (subtract vb va)

    let sum (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)

    let div divisor (a1, a2, a3) =
        a1 / divisor, a2 / divisor, a3 / divisor

    let multiply multiplier (a1, a2, a3) =
        (a1 * multiplier, a2 * multiplier, a3 * multiplier)

    let cross (a1, a2, a3) (b1, b2, b3) =
        (a2 * b3 - a3 * b2), (a3 * b1 - a1 * b3), (a1 * b2 - a2 * b1)

    let dot ((a1, a2, a3): int64 * int64 * int64) ((b1, b2, b3): int64 * int64 * int64) =
        let x = bigint.Multiply(a1, b1)
        let y = bigint.Multiply(a2, b2)
        let z = bigint.Multiply(a3, b3)
        bigint.Add(x, y) |> fun xy -> bigint.Add(xy, z)

    let part2 (input: string seq) =
        let rays =
            input
            |> List.ofSeq
            |> List.map (fun x ->
                let split = x.Split('@', StringSplitOptions.TrimEntries)
                strToTuple split[0] int64, strToTuple split[1] int64)

        let a = List.head rays
        let bcd = rays |> List.tail |> List.take 3 |> List.map (diff a)
        let b0 = fst bcd[0]
        let b1 = snd bcd[0] |> sum (fst bcd[0])

        // Normal of the plane
        let normal = cross b0 b1

        let xc, yc, zc = fst bcd[1]
        // Time when Hailstone C intersects the plane
        let tc = dot (-xc, -yc, -zc) normal / dot normal (snd bcd[1]) |> int64
        // Intersection point of C, Ic
        let ic = snd bcd[1] |> multiply tc |> sum (fst bcd[1])

        // Same thing for Hailstone D
        let xd, yd, zd = fst bcd[2]
        let td = dot (-xd, -yd, -zd) normal / dot normal (snd bcd[2]) |> int64
        let id = snd bcd[2] |> multiply td |> sum (fst bcd[2])

        // Rock's vector Vr. Get diff of Hailstone C and D's intersection points, and then divide it by the time difference
        let vxr, vyr, vzr = subtract id ic |> div (td - tc)

        // Calculate rock's position Pr + t * Vr = Ic.
        let ixc, iyc, izc = ic
        let xr = ixc - (tc * vxr)
        let yr = iyc - (tc * vyr)
        let zr = izc - (tc * vzr)
        // Finally add the Hailstone A's position to get the real position of the rock.
        let pxr, pyr, pzr = sum (xr, yr, zr) (fst a)

        pxr + pyr + pzr
