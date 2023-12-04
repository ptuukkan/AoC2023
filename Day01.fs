namespace AoC2023

open System

module Day01 =
    let numberWords =
        seq {
            "one"
            "two"
            "three"
            "four"
            "five"
            "six"
            "seven"
            "eight"
            "nine"
        }

    let firstNumberWord (input: string) =
        numberWords
        |> Seq.mapi (fun i word -> input.IndexOf(word), i + 1 |> string)
        |> Seq.filter (fun (index, _) -> index <> -1)
        |> Seq.sort
        |> Seq.tryHead

    let firstNumberChar (input: string) =
        input.ToCharArray()
        |> Array.tryFindIndex Char.IsNumber
        |> Option.bind (fun index -> Some(index, input[index] |> string))

    let lastNumberWord (input: string) =
        numberWords
        |> Seq.mapi (fun i word -> input.LastIndexOf(word), i + 1 |> string)
        |> Seq.filter (fun (index, _) -> index <> -1)
        |> Seq.sortDescending
        |> Seq.tryHead

    let lastNumberChar (input: string) =
        input.ToCharArray()
        |> Array.tryFindIndexBack Char.IsNumber
        |> Option.bind (fun index -> Some(index, input[index] |> string))

    let firstNumber (input: string) =
        [| firstNumberWord input; firstNumberChar input |]
        |> Array.choose id
        |> Array.sort
        |> Array.head
        |> snd

    let lastNumber (input: string) =
        [| lastNumberWord input; lastNumberChar input |]
        |> Array.choose id
        |> Array.sortDescending
        |> Array.head
        |> snd

    let part1 (input: string seq) =
        input
        |> Seq.map _.ToCharArray()
        |> Seq.map (fun x -> x |> Array.find Char.IsNumber, x |> Array.findBack Char.IsNumber)
        |> Seq.map (fun (a, b) -> String [| a; b |])
        |> Seq.map int
        |> Seq.sum

    let part2 (input: string seq) =
        input
        |> Seq.map (fun x -> String.Join("", [| firstNumber x; lastNumber x |]))
        |> Seq.map int
        |> Seq.sum
