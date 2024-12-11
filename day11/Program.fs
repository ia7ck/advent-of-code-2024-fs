open Xunit
open FsUnit.Xunit

let blink stone =
    if stone = 0L then
        [ 1L ]
    else
        let s = stone.ToString()

        if s.Length % 2 = 0 then
            [ int64 s[.. (s.Length / 2 - 1)]; int64 s[s.Length / 2 ..] ]
        else
            [ stone * 2024L ]

let part1 (stones: int64 seq) =
    (List.ofSeq stones, [ 1..25 ])
    ||> List.fold (fun stones _ -> stones |> List.collect blink)
    |> List.length

let part2 (stones: int64 seq) =
    let stones = stones |> Seq.map (fun x -> (x, 1L)) |> Map.ofSeq

    (stones, [ 1..75 ])
    ||> List.fold (fun stones _ ->
        (Map.empty, stones)
        ||> Map.fold (fun acc k v ->
            (acc, blink k)
            ||> List.fold (fun acc newK ->
                let newV = v + (acc |> Map.tryFind newK |> Option.defaultValue 0L)
                acc |> Map.add newK newV)))
    |> Map.values
    |> Seq.sum

let parse (input: string) = input.Split(" ") |> Seq.map int64

module Example =
    let input = "125 17"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 55312

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let stones = parse input

    stones |> part1 |> printfn "Part 1: %d"
    stones |> part2 |> printfn "Part 2: %d"

    0
