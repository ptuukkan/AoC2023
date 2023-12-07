namespace AoC2023

module Day05 =
    type MapLine = uint * uint * uint
    type SeedRange = uint * uint

    let createMaps (maps: (MapLine) array array) (line: string) =
        if line.EndsWith("map:") then
            Array.append maps [| [||] |]
        else
            let currentMap = maps |> Array.tryLast |> Option.defaultValue [||]
            let mapLine = line.Split(' ') |> Array.map uint

            let newCurrentMap =
                [| mapLine[0], mapLine[1], mapLine[2] |] |> Array.append currentMap

            if Array.isEmpty maps then
                Array.append maps [| newCurrentMap |]
            else
                maps[maps.Length - 1] <- newCurrentMap
                maps

    let parseInput (input: string seq) =
        let seeds =
            input
            |> (Seq.head >> _.Split(':') >> Array.last)
            |> (_.Trim() >> _.Split(' ') >> Array.map uint)

        let maps =
            input
            |> Seq.skip 2
            |> Seq.filter (fun x -> x.Length > 0)
            |> Seq.fold createMaps [||]

        seeds, maps

    let convertToRanges (seeds: uint array) =
        (([||], None), seeds)
        ||> Array.fold (fun (seedRanges, previousSeed) currentSeed ->
            match previousSeed with
            | Some seed -> [| seed, currentSeed |] |> (Array.append seedRanges), None
            | None -> seedRanges, Some currentSeed)
        |> fst


    let mapSeed (seed: uint) (map: MapLine array) =
        map
        |> Array.tryFind (fun (_dst, src, len) -> seed >= src && seed < src + len)
        |> Option.bind (fun (dst, src, _len) -> Some(dst + seed - src))
        |> Option.defaultValue seed

    let isOverlap (seedStart: uint) (seedLength: uint) (mapStart: uint) (mapLength: uint) =
        let seedEnd = seedStart + seedLength - 1u
        let mapEnd = mapStart + mapLength - 1u

        (seedStart < mapStart && seedEnd >= mapStart)
        || (seedStart <= mapEnd && seedEnd > mapEnd)

    let isInside (seedStart: uint) (seedLength: uint) (mapStart: uint) (mapLength: uint) =
        let seedEnd = seedStart + seedLength - 1u
        let mapEnd = mapStart + mapLength - 1u
        seedStart >= mapStart && seedEnd <= mapEnd

    let mapSeedRange (map: MapLine array) (seedRange: SeedRange) : SeedRange array =
        let rec mapSeedRangeInner
            (map: MapLine array)
            (mapped: SeedRange array)
            (unMapped: SeedRange array)
            : SeedRange array =

            if Array.isEmpty unMapped then
                mapped
            else
                match Array.tryHead map with
                | None -> Array.append mapped unMapped
                | Some(dst, src, len) ->
                    unMapped
                    |> Array.map (fun (seedStart, seedLength) ->
                        if isOverlap seedStart seedLength src len then
                            if seedStart < src then
                                [| seedStart, src - seedStart; src, seedLength - (src - seedStart) |]
                                |> mapSeedRangeInner map mapped
                            else
                                [| seedStart, src + len - seedStart
                                   src + len, seedStart + seedLength - (src + len) |]
                                |> mapSeedRangeInner map mapped
                        else if isInside seedStart seedLength src len then
                            mapSeedRangeInner (Array.tail map) [| seedStart + dst - src, seedLength |] [||]
                        else
                            mapSeedRangeInner (Array.tail map) [||] [| seedStart, seedLength |])
                    |> Array.concat

        mapSeedRangeInner map [||] [| seedRange |]


    let mapSeedRanges (maps: MapLine array array) (seedRange: SeedRange) =
        ([| seedRange |], maps)
        ||> Array.fold (fun ranges map -> ranges |> Array.map (mapSeedRange map) |> Array.concat)

    let part1 (input: string seq) =
        let seeds, maps = parseInput input

        seeds
        |> Array.map (fun seed -> (seed, maps) ||> Array.fold mapSeed)
        |> Array.min

    let part2 (input: string seq) =
        let seeds, maps = parseInput input

        seeds
        |> convertToRanges
        |> Array.map (mapSeedRanges maps)
        |> Array.concat
        |> Array.map fst
        |> Array.min
