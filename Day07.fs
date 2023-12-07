namespace AoC2023

open System

module Day07 =
    type Hand =
        | HighCard = 1
        | OnePair = 2
        | TwoPairs = 3
        | Threes = 4
        | FullHouse = 5
        | Fours = 6
        | Fives = 7

    let cardValues =
        Map
            [ '2', "2"
              '3', "3"
              '4', "4"
              '5', "5"
              '6', "6"
              '7', "7"
              '8', "8"
              '9', "9"
              'T', "A"
              'J', "B"
              'Q', "C"
              'K', "D"
              'A', "E" ]

    let cardValuesPart2 =
        Map
            [ '2', "2"
              '3', "3"
              '4', "4"
              '5', "5"
              '6', "6"
              '7', "7"
              '8', "8"
              '9', "9"
              'T', "A"
              'J', "1"
              'Q', "C"
              'K', "D"
              'A', "E" ]

    let hexToInt (str: string) : int = Convert.ToInt32(str, 16)

    let parseHands (input: string seq) =
        input |> Seq.map _.Split(' ') |> Seq.map (fun x -> x[0], int x[1])

    let identifyHand ((hand, bid): string * int) =
        let cardGroups =
            hand.ToCharArray()
            |> Array.groupBy id
            |> Array.map (fun (key, elems) -> Array.length elems)
            |> Array.sortDescending

        let first = Array.head cardGroups
        let second = Array.tryItem 1 cardGroups

        let handType =
            match first, second with
            | 5, _ -> Hand.Fives
            | 4, _ -> Hand.Fours
            | 3, Some(2) -> Hand.FullHouse
            | 3, _ -> Hand.Threes
            | 2, Some(2) -> Hand.TwoPairs
            | 2, _ -> Hand.OnePair
            | 1, _ -> Hand.HighCard
            | _, _ -> raise (Exception("Unidentified hand"))

        (hand, handType, bid)

    let identifyHandPart2 ((hand, bid): string * int) =
        let cardGroups =
            hand.ToCharArray()
            |> Array.groupBy id
            |> Array.map (fun (key, elems) -> key, Array.length elems)
            |> Array.sortByDescending snd

        let jokers =
            cardGroups
            |> Array.tryFind (fun (card, count) -> card = 'J')
            |> Option.map snd
            |> Option.defaultValue 0

        let groupsWithoutJokers =
            cardGroups |> Array.filter (fun (card, count) -> card <> 'J') |> Array.map snd

        let first =
            match Array.tryHead groupsWithoutJokers with
            | Some x -> x + jokers
            | None -> 5

        let second = Array.tryItem 1 groupsWithoutJokers

        let handType =
            match first, second with
            | 5, _ -> Hand.Fives
            | 4, _ -> Hand.Fours
            | 3, Some(2) -> Hand.FullHouse
            | 3, _ -> Hand.Threes
            | 2, Some(2) -> Hand.TwoPairs
            | 2, _ -> Hand.OnePair
            | 1, _ -> Hand.HighCard
            | _, _ -> raise (Exception("Unidentified hand"))

        (hand, handType, bid)

    let handValue (values: Map<char, string>) ((hand, handType, bid): string * Hand * int) =
        let cardValues =
            hand.ToCharArray() |> Array.map (fun x -> values[x]) |> Array.reduce (+)

        hexToInt ((int handType).ToString("X") + cardValues)

    let part1 (input: string seq) =
        input
        |> parseHands
        |> Seq.map identifyHand
        |> Seq.sortBy (handValue cardValues)
        |> Seq.mapi (fun i (a, b, bid) -> (1 + i) * bid)
        |> Seq.reduce (+)

    let part2 (input: string seq) =
        input
        |> parseHands
        |> Seq.map identifyHandPart2
        |> Seq.sortBy (handValue cardValuesPart2)
        |> Seq.mapi (fun i (a, b, bid) -> (1 + i) * bid)
        |> Seq.reduce (+)
