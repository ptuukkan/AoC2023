namespace AoC2023

open System.Text.RegularExpressions

module Day02 =
    let highestAmountOfCubesByColor (game: string) (color: string) =
        Regex.Matches(game, $"(\d+)\s{color}")
        |> Seq.map (_.Groups >> Seq.last >> _.Value >> int)
        |> Seq.max

    let isPossible (game: string) =
        let cubes = highestAmountOfCubesByColor game

        (cubes "red") <= 12 && (cubes "green") <= 13 && (cubes "blue") <= 14

    let getGameId (game: string) =
        Regex.Match(game, "^Game\s(\d+)").Groups
        |> Seq.last
        |> (_.Value)
        |> int

    let getMinimumAmountOfCubes (game: string) =
        let cubes = highestAmountOfCubesByColor game

        seq {
            cubes "red"
            cubes "green"
            cubes "blue"
        }

    let part1 (input: string seq) =
        input
        |> Seq.filter isPossible
        |> Seq.map getGameId
        |> Seq.sum

    let part2 (input: string seq) =
        input
        |> Seq.map getMinimumAmountOfCubes
        |> Seq.map (Seq.reduce (*))
        |> Seq.sum
