open Xunit
open FsUnit.Xunit

let isSafe (report: int seq) =
    let pairs = Seq.pairwise report
    let inc = fun (x, y) -> x < y
    let dec = fun (x, y) -> x > y
    let geq1 = fun (x, y) -> abs (x - y) >= 1
    let leq3 = fun (x, y) -> abs (x - y) <= 3

    (Seq.forall inc pairs || Seq.forall dec pairs)
    && (Seq.forall (fun p -> geq1 p && leq3 p) pairs)

let part1 (reports: int seq seq) =
    reports |> Seq.filter isSafe |> Seq.length

let part2 (reports: int seq seq) =
    reports
    |> Seq.filter (fun report ->
        [ 0 .. (Seq.length report) - 1 ]
        |> Seq.exists (fun i -> isSafe (Seq.removeAt i report)))
    |> Seq.length

let parse (input: string) =
    input.Split("\n") |> Seq.map (fun line -> line.Split(" ") |> Seq.map int)

module Example =
    let input =
        "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 2

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 4

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let reports = parse input

    reports |> part1 |> printfn "Part 1: %d"
    reports |> part2 |> printfn "Part 2: %d"

    0
