open Xunit
open FsUnit.Xunit

let solve n positions =
    let positions = Set.ofSeq positions

    let transition (x, y) dist =
        let newNodes, newDist =
            (dist, [ (-1, 0); (0, -1); (1, 0); (0, 1) ])
            ||> List.mapFold (fun dist (dx, dy) ->
                let nx, ny = x + dx, y + dy

                if
                    0 <= nx
                    && nx <= n
                    && 0 <= ny
                    && ny <= n
                    && not (Set.contains (nx, ny) positions)
                    && not (Map.containsKey (nx, ny) dist)
                then
                    (Some(nx, ny), dist |> Map.add (nx, ny) (dist[(x, y)] + 1))
                else
                    (None, dist))

        (List.choose id newNodes, newDist)

    let rec bfs nodes dist =
        if List.isEmpty nodes then
            dist
        else
            let newNodes, newDist =
                (dist, nodes) ||> List.mapFold (fun dist node -> transition node dist)

            bfs (List.concat newNodes) newDist

    let dist = bfs [ (0, 0) ] (Map [ ((0, 0), 0) ])

    Map.tryFind (n, n) dist

let part1 ((n, bytes, positions): (int * int * (int * int) seq)) =
    solve n (Seq.take bytes positions) |> Option.get

let part2 ((n, positions): (int * (int * int) seq)) =
    let positions = List.ofSeq positions

    let rec bisect ok ng =
        if ok + 1 = ng then
            ng
        else
            let mid = (ok + ng) / 2

            if solve n positions[..mid] |> Option.isSome then
                bisect mid ng
            else
                bisect ok mid

    let i = bisect 0 positions.Length
    positions[i]

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun line ->
        let line = line.Split(",")
        (int line.[0], int line.[1]))

module Example =
    let input =
        "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"

    [<Fact>]
    let testPart1 () =
        (6, 12, parse input) |> part1 |> should equal 22

    [<Fact>]
    let testPart2 () =
        (6, parse input) |> part2 |> should equal (6, 1)

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let positions = parse input

    (70, 1024, positions) |> part1 |> printfn "Part 1: %d"
    (70, positions) |> part2 |> printfn "Part 2: %A"

    0
