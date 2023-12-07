namespace AoC2023

open System.Text.RegularExpressions

module Day06 =
    let parseRaces (input: string seq) =
        input
        |> Array.ofSeq
        |> Array.map (_.Split(':') >> Array.last >> _.Trim())
        |> Array.map (fun x -> Regex.Split(x, "\s+"))
        |> Array.map (Array.map int)
        |> Array.transpose
        |> Array.map (fun x -> x[0], x[1])
        
    let parseRace (input: string seq) =
        input
        |> Array.ofSeq
        |> Array.map (_.Split(':') >> Array.last >> _.Replace(" ", ""))
        |> Array.map uint64
        |> (fun x -> x[0], x[1])
        
    let race (time: int, distance: int) =
        [|0..time|]
        |> Array.map (fun speed -> (time - speed) * speed)
        |> Array.filter (fun x -> x > distance)
    
    let raceUint (time: uint64, distance: uint64) =
        [|0UL..time|]
        |> Array.map (fun speed -> (time - speed) * speed)
        |> Array.filter (fun x -> x > distance)
        
    let part1 (input: string seq) =
        input
        |> parseRaces
        |> Array.map race
        |> Array.map Array.length
        |> Array.reduce (*)
        
    let part2 (input: string seq) =
        input
        |> parseRace
        |> raceUint
        |> Array.length
