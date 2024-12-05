open Xunit
open FsUnit.Xunit

let splitUpdates (rules: (int * int) seq) (updates: int seq seq) =
    let pages = rules |> Seq.collect (fun (p, q) -> [ p; q ]) |> Seq.distinct

    Seq.allPairs pages pages
    |> Seq.iter (fun (p, q) ->
        // 全ペアが rules に出てくる
        Assert.True(p = q || Seq.contains (p, q) rules || Seq.contains (q, p) rules))

    let following =
        (Map.empty, rules)
        ||> Seq.fold (fun acc (p, q) ->
            let v = acc |> Map.tryFind p |> Option.defaultValue Set.empty
            Map.add p (Set.add q v) acc)

    let correct, incorrect =
        updates
        |> List.ofSeq
        |> List.map List.ofSeq
        |> List.partition (fun updates ->
            updates
            |> List.indexed
            |> List.forall (fun (i, p) ->
                List.skip (i + 1) updates
                |> List.forall (fun q ->
                    let ``p,q`` =
                        Map.tryFind p following
                        |> Option.map (fun v -> Set.contains q v)
                        |> Option.defaultValue true

                    let ``q,p`` =
                        Map.tryFind q following
                        |> Option.map (fun v -> Set.contains p v |> not)
                        |> Option.defaultValue true

                    ``p,q`` && ``q,p``)))

    (following, (correct, incorrect))

let part1 (rules: (int * int) seq) (updates: int seq seq) =
    let _, (correctUpdates, _) = splitUpdates rules updates

    correctUpdates |> List.sumBy (fun updates -> updates[updates.Length / 2])

let part2 (rules: (int * int) seq) (updates: int seq seq) =
    let following, (_, incorrectUpdates) = splitUpdates rules updates

    incorrectUpdates
    |> List.sumBy (fun updates ->
        let sorted =
            updates
            |> List.sortWith (fun p q ->
                following
                |> Map.tryFind p
                |> Option.map (fun v -> if Set.contains q v then -1 else 1)
                |> Option.defaultValue 1)

        sorted[sorted.Length / 2])

let parse (input: string) =
    let input = input.Split("\n\n")
    let first, second = input[0], input[1]

    let rules =
        first.Split("\n")
        |> Seq.map (fun line ->
            let line = line.Split('|')
            (int line[0], int line[1]))

    let updates =
        second.Split("\n") |> Seq.map (fun line -> line.Split(",") |> Seq.map int)

    (rules, updates)

module Example =
    let input =
        "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

    [<Fact>]
    let testPart1 () =
        parse input ||> part1 |> should equal 143

    [<Fact>]
    let testPart2 () =
        parse input ||> part2 |> should equal 123

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let rules, updates = parse input

    (rules, updates) ||> part1 |> printfn "Part 1: %d"
    (rules, updates) ||> part2 |> printfn "Part 2: %d"

    0
