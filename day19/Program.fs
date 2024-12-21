open System.Collections.Generic

open Xunit
open FsUnit.Xunit

let part1 ((patterns, designs): (string seq * string seq)) =
    let rec search (design: string) =
        if design.Length = 0 then
            true
        else
            patterns
            |> Seq.exists (fun pattern -> design.StartsWith(pattern) && search design[pattern.Length ..])

    designs |> Seq.filter search |> Seq.length

let part2 ((patterns, designs): (string seq * string seq)) =
    let memo = Dictionary()

    let rec solve (design: string) =
        if design.Length = 0 then
            1L
        else
            match memo.TryGetValue(design) with
            | true, value -> value
            | false, _ ->
                let value =
                    patterns
                    |> Seq.sumBy (fun pattern ->
                        if design.StartsWith(pattern) then
                            solve design[pattern.Length ..]
                        else
                            0)

                memo.Add(design, value)
                value

    designs |> Seq.sumBy solve



let parse (input: string) =
    let input = input.Split("\n\n")
    let patterns = input[0].Split(", ")
    let designs = input[1].Split("\n")

    patterns, designs

module Example =
    let input =
        "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 6

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 16

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let (patterns, designs) = parse input

    (patterns, designs) |> part1 |> printfn "Part 1: %d"
    (patterns, designs) |> part2 |> printfn "Part 2: %d"

    0
