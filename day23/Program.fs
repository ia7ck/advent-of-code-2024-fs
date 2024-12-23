open Xunit
open FsUnit.Xunit

let makeGraph edges =
    (Map.empty, edges)
    ||> Seq.fold (fun acc (u, v) ->
        acc
        |> Map.change u (function
            | Some x -> Some(v :: x)
            | None -> Some [ v ])
        |> Map.change v (function
            | Some x -> Some(u :: x)
            | None -> Some [ u ]))

let part1 (edges: (string * string) seq) =
    let graph = makeGraph edges
    let nodes = graph |> Map.keys |> Seq.distinct
    let edges = Set.ofSeq edges

    nodes
    |> Seq.collect (fun x ->
        graph[x]
        |> List.collect (fun y ->
            graph[y]
            |> List.filter (fun z -> Set.contains (x, z) edges || Set.contains (z, x) edges)
            |> List.map (fun z -> (y, z)))
        |> List.map (fun (y, z) -> (x, y, z)))
    |> Seq.filter (fun (x, y, z) -> x.StartsWith('t') || y.StartsWith('t') || z.StartsWith('t'))
    |> Seq.length
    |> (fun len -> len / 6)

let part2 (edges: (string * string) seq) =
    let graph = makeGraph edges
    let edges = List.ofSeq edges
    let edgesSet = Set.ofSeq edges

    let rec find cliques =
        let newCliques =
            cliques
            |> List.collect (fun clique ->
                let this = Set.ofList clique

                graph[clique[0]]
                |> List.choose (fun v ->
                    if
                        Set.contains v this |> not
                        && List.forall (fun x -> Set.contains (x, v) edgesSet || Set.contains (v, x) edgesSet) clique
                    then
                        Some(v :: clique)
                    else
                        None))
            |> List.distinctBy List.sort

        if List.isEmpty newCliques then cliques else find newCliques


    edges
    |> List.map (fun (u, v) -> [ u; v ])
    |> find
    |> List.head
    |> List.sort
    |> String.concat ","

let parse (input: string) =
    input.Split("\n")
    |> Array.map (fun line ->
        let line = line.Split("-")
        (line.[0], line.[1]))

module Example =
    let input =
        "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 7

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal "co,de,ka,ta"

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let edges = parse input

    edges |> part1 |> printfn "Part 1: %d"
    edges |> part2 |> printfn "Part 2: %s"

    0
