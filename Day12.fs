namespace AoC2023

open System
open System.Collections.Generic

module Day12 =
    let parseInput (str: string) =
        let split = str.Split(' ', StringSplitOptions.RemoveEmptyEntries)

        split[0].Trim().ToCharArray() |> List.ofArray,
        split[1].Trim().Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map int

    let expand ((record, groups): char list * int list) =
        record
        |> List.replicate 5
        |> List.reduce (fun a b -> b |> List.append [ '?' ] |> List.append a),
        groups |> List.replicate 5 |> List.concat

    let isFirstAndTethered (record: char list) (group: int) (index: int) =
        if index <> 0 then
            false, 0
        else
            match record |> List.tryFindIndex (fun x -> x = '#') with
            | Some(index) -> if index - 1 >= group then false, 0 else true, index
            | None -> false, 0

    let isLastAndTethered (record: char list) (group: int) (isLast: bool) =
        if isLast |> not then
            false, 0
        else
            match record |> List.tryFindIndexBack (fun x -> x = '#') with
            | Some(index) ->
                if record.Length - (index + 2) >= group then
                    false, record.Length - 1
                else
                    true, index
            | None -> false, record.Length - 1

    let groupSlices ((record, groups): char list * int list) =
        let groupsWithSlices =
            groups
            |> List.mapi (fun i g ->
                let firstTether, firstDamaged = isFirstAndTethered record g i
                let lastTether, lastDamaged = isLastAndTethered record g (i = groups.Length - 1)

                let startIndex =
                    match i with
                    | 0 -> if firstTether then firstDamaged - (g - 1) else 0
                    | _ -> groups[.. (i - 1)] |> List.sum |> (+) i

                let endIndex =
                    match i with
                    | 0 when firstTether -> min (record.Length - 1) (firstDamaged + (g - 1))
                    | last when i = (groups.Length - 1) ->
                        if lastTether then
                            lastDamaged + (g - 1) |> min (record.Length - 1)
                        else
                            record.Length - 1
                    | _ -> groups[i + 1 ..] |> List.sum |> (+) (groups.Length - i) |> (-) record.Length

                g, startIndex, endIndex)

        record, groupsWithSlices

    let deriveFirstIndex (startIndex: int) (damagedIndex: int) (groupLength: int) =
        (groupLength - 1) |> (-) (startIndex + damagedIndex) |> max startIndex

    let deriveLastIndex (startIndex: int) (damagedIndex: int) (groupLength: int) (recordLength: int) =
        (groupLength - 1) |> (+) (startIndex + damagedIndex) |> min (recordLength - 1)

    let memoize f =
        let dict = Dictionary<_, _>()

        fun c ->
            let exist, value = dict.TryGetValue c

            match exist with
            | true -> value
            | _ ->
                let value = f c
                dict.Add(c, value)
                value

    let possibleArrangements ((record, groups): char list * (int * int * int) list) =
        let rec inner =
            memoize (fun ((groups, prev_len, prev_index): (int * int * int) list * int * int) ->
                match groups |> List.tryHead with
                | Some(len, s, e) ->

                    let adjusted_s, adjusted_e =
                        let temp_s = max s (prev_index + prev_len + 1)
                        let first = record[temp_s..e] |> List.tryFindIndex (fun x -> x = '#')

                        if groups.Length = 1 then
                            let last = record[temp_s..e] |> List.tryFindIndexBack (fun x -> x = '#')

                            match first, last with
                            | Some f, Some l when l - f >= len -> // no matches
                                record.Length, record.Length
                            | Some f, Some l ->
                                deriveFirstIndex temp_s f (len - (l - f)),
                                deriveLastIndex temp_s l (len - (l - f)) record.Length
                            | _, _ -> temp_s, e
                        else
                            match first with
                            | None -> temp_s, e
                            | Some index when index > len -> // not tether
                                temp_s, deriveLastIndex temp_s index len record.Length
                            | Some index -> // tether
                                deriveFirstIndex temp_s index len, deriveLastIndex temp_s index len record.Length

                    let possiblePositions =
                        record[adjusted_s..adjusted_e]
                        |> List.indexed
                        |> List.windowed len
                        |> List.filter (fun window ->
                            let damaged = window |> List.filter (fun (i, c) -> c = '#') |> List.length
                            let slots = window |> List.filter (fun (i, c) -> c = '?') |> List.length
                            damaged + slots = len)
                        |> List.filter (fun window -> // Next character must not be #
                            let nextIndex = window |> List.last |> fst |> (+) adjusted_s |> (+) 1

                            match record |> List.tryItem nextIndex with
                            | Some c -> c <> '#'
                            | None -> true)
                        |> List.filter (fun window -> // Prev character must not be #
                            let prevIndex = window |> List.head |> fst |> (+) adjusted_s

                            match record |> List.tryItem (prevIndex - 1) with
                            | Some c -> c <> '#'
                            | None -> true)
                        |> List.map (fun x -> x |> List.head |> fst |> (+) adjusted_s)


                    possiblePositions
                    |> List.map (fun x -> (inner ((groups |> List.tail), len, x)))
                    |> List.sum

                | None -> 1UL)

        inner (groups, -1, 0)

    let part1 (input: string seq) =
        input
        |> List.ofSeq
        |> List.map parseInput
        |> List.map groupSlices
        |> List.map possibleArrangements
        |> List.sum

    let part2 (input: string seq) =
        input
        |> List.ofSeq
        |> List.map parseInput
        |> List.map expand
        |> List.map groupSlices
        |> List.map possibleArrangements
        |> List.sum
