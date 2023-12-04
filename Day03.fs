namespace AoC2023

open System
open System.Text.RegularExpressions

module Day03 =
    type Symbol = { value: char; x: int; y: int }

    type PartNumber =
        { value: int
          length: int
          x: int
          y: int }

    let getSymbols (y: int) (line: char array) =
        line
        |> Array.indexed
        |> Array.filter (fun (i, c) -> Regex.IsMatch(c.ToString(), "[^\d\s\.]"))
        |> Array.map (fun (i, c) -> { value = c; x = i; y = y })

    let getPartNumbers (y: int) (line: char array) =
        let indexed = line |> Array.indexed

        indexed
        |> Array.filter (fun (i, n) -> Char.IsNumber(n) && (i = 0 || Char.IsNumber(snd indexed[i - 1]) |> not))
        |> Array.map (fun (i, n) ->
            let partNumber =
                line[i..]
                |> Array.takeWhile Char.IsNumber
                |> Array.map Char.ToString
                |> String.concat ""

            { x = i
              y = y
              value = int partNumber
              length = partNumber.Length })

    let hasAdjacentSymbol (symbols: Symbol seq) (partNumber: PartNumber) =
        symbols
        |> Seq.filter (fun symbol -> abs (symbol.y - partNumber.y) < 2)
        |> Seq.filter (fun symbol -> symbol.x - partNumber.x > -2)
        |> Seq.filter (fun symbol -> symbol.x - partNumber.x <= partNumber.length)
        |> Seq.isEmpty
        |> not

    let getAdjacentParts (parts: PartNumber seq) (gear: Symbol) =
        parts
        |> Seq.filter (fun part -> abs (gear.y - part.y) < 2)
        |> Seq.filter (fun part -> gear.x - part.x > -2)
        |> Seq.filter (fun part -> gear.x - part.x <= part.length)

    let part1 (input: string seq) =
        let schematic = input |> Seq.map _.ToCharArray()
        let symbols = schematic |> Seq.mapi getSymbols |> Seq.concat |> Seq.toArray

        schematic
        |> Seq.mapi getPartNumbers
        |> Seq.concat
        |> Seq.filter (hasAdjacentSymbol symbols)
        |> Seq.map _.value
        |> Seq.sum

    let part2 (input: string seq) =
        let schematic = input |> Seq.map _.ToCharArray()
        let partNumbers = schematic |> Seq.mapi getPartNumbers |> Seq.concat |> Seq.toArray

        schematic
        |> Seq.mapi getSymbols
        |> Seq.concat
        |> Seq.filter (fun x -> x.value = '*')
        |> Seq.map (getAdjacentParts partNumbers)
        |> Seq.filter (fun x -> Seq.length x = 2)
        |> Seq.map (Seq.map _.value)
        |> Seq.map (Seq.reduce (*))
        |> Seq.sum
