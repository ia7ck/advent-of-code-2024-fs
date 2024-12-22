open System.Diagnostics

open Xunit
open FsUnit.Xunit

let mix value secret = value ^^^ secret

let prune secret = secret % 16777216L

let next secret =
    let secret = secret |> mix (secret * 64L) |> prune
    let secret = secret |> mix (secret / 32L) |> prune
    secret |> mix (secret * 2048L) |> prune

let rec nthNext n secret =
    if n = 0 then secret else next secret |> nthNext (n - 1)

let part1 (initialSecrets: int64 seq) =
    initialSecrets |> Seq.sumBy (fun secret -> nthNext 2000 secret)

let part2 (initialSecrets: int64 seq) =
    let prices =
        List.ofSeq initialSecrets
        |> List.map (fun secret ->
            let secrets = [ 0..2000 ] |> List.map (fun n -> nthNext n secret)

            let changes =
                secrets |> List.pairwise |> List.map (fun (s, s') -> s' % 10L - s % 10L)

            (Map.empty, changes |> List.windowed 4 |> List.indexed)
            ||> List.fold (fun acc (i, w) ->
                acc
                |> Map.change w (function
                    | Some p -> Some p
                    | None -> Some(secrets[i + 4] % 10L))))

    [ -9 .. 9 ]
    |> List.allPairs [ -9 .. 9 ]
    |> List.allPairs [ -9 .. 9 ]
    |> List.allPairs [ -9 .. 9 ]
    |> List.map (fun (a, (b, (c, d))) ->
        prices
        |> List.sumBy (fun prices -> prices |> Map.tryFind [ a; b; c; d ] |> Option.defaultValue 0))
    |> List.max

let parse (input: string) = input.Split("\n") |> Array.map int64

module Example =
    let input1 =
        "1
10
100
2024"

    let input2 =
        "1
2
3
2024"

    [<Fact>]
    let testPart1 () =
        parse input1 |> part1 |> should equal 37327623L

    [<Fact>]
    let testPart2 () =
        parse input2 |> part2 |> should equal 23L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let initialSecrets = parse input

    initialSecrets |> part1 |> printfn "Part 1: %d"

    let timer = Stopwatch()
    timer.Start()
    initialSecrets |> part2 |> printfn "Part 2: %d"
    timer.Stop()

    printfn "elapsed = %A" timer.Elapsed // 00:03:57.9471839

    0
