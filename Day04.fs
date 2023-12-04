namespace AoC2023

open System.Text.RegularExpressions

module Day04 =
    let splitString (line: string) =
        line.Split(':')
        |> Array.last
        |> _.Split('|')
        |> Array.map (fun x -> Regex.Split(x.Trim(), "\s+") |> Array.map int)

    let matchWinningNumbers (numbers: int array array) =
        numbers |> Seq.map Set.ofArray |> Set.intersectMany |> Set.toArray

    let calculatePoints (numbers: int array) =
        match numbers.Length with
        | 0 -> 0
        | 1 -> 1
        | n -> pown 2 (n - 1)

    let part1 (input: string seq) =
        input
        |> Seq.map splitString
        |> Seq.map matchWinningNumbers
        |> Seq.map calculatePoints
        |> Seq.sum

    let part2 (input: string seq) =
        let scratchCards = input |> Seq.map splitString |> Seq.toArray

        let copies =
            scratchCards
            |> Array.map (matchWinningNumbers >> Array.length)
            |> Array.mapi (fun i x -> [| i + 1 .. x + i |])
            |> Array.indexed
        
        ([||], copies)
        ||> Array.fold (fun cardCopies (cardId, cardsToCopy) ->
            let copyAmount =
                cardCopies
                |> Array.filter (fun x -> x = cardId)
                |> Array.length
                |> (+) 1

            cardsToCopy
            |> Array.replicate copyAmount
            |> Array.concat
            |> Array.append cardCopies)
        |> Array.length
        |> (+) (Array.length scratchCards)