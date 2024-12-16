open Xunit
open FsUnit.Xunit

type Dir =
    | E
    | N
    | W
    | S

    member this.CCW() =
        match this with
        | E -> N
        | N -> W
        | W -> S
        | S -> E

    member this.CW() =
        match this with
        | E -> S
        | N -> E
        | W -> N
        | S -> W

    member this.Rev() = this.CCW().CCW()

type Node =
    { Row: int
      Col: int
      Dir: Dir }

    member this.Forward(maze: char[][]) =
        let nRow, nCol =
            match this.Dir with
            | E -> this.Row, this.Col + 1
            | N -> this.Row - 1, this.Col
            | W -> this.Row, this.Col - 1
            | S -> this.Row + 1, this.Col

        if
            0 <= nRow
            && nRow < maze.Length
            && 0 <= nCol
            && nCol < maze[nRow].Length
            && maze[nRow][nCol] <> '#'
        then
            Some { this with Row = nRow; Col = nCol }
        else
            None

    member this.Rotate() =
        [ { this with Dir = this.Dir.CCW() }; { this with Dir = this.Dir.CW() } ]

type Edge = { From: Node; To: Node; Cost: int }

let dijkstra (start: Node) (edges: Edge list) =
    let adj = edges |> List.groupBy (fun e -> e.From) |> Map

    let relax (from: Node) (e: Edge) (dist: Map<Node, int>) =
        let d = dist[from]
        let newD = d + e.Cost

        match Map.tryFind e.To dist with
        | Some d' when newD < d' -> dist |> Map.add e.To newD |> Some
        | None -> dist |> Map.add e.To newD |> Some
        | _ -> None

    let transition (node: Node) dist =
        let newNodes, dist =
            (dist, adj[node])
            ||> List.mapFold (fun dist e ->
                match relax node e dist with
                | Some dist -> (Some e.To, dist)
                | None -> (None, dist))

        (List.choose id newNodes, dist)

    let rec solve (nodes: Node list) dist =
        if List.isEmpty nodes then
            dist
        else
            let newNodes, newDist =
                (dist, nodes) ||> List.mapFold (fun dist node -> transition node dist)

            solve (List.concat newNodes) newDist

    solve [ start ] (Map [ (start, 0) ])

let collectEdges (maze: char[][]) =
    List.allPairs [ 0 .. (maze.Length - 1) ] [ 0 .. (maze[0].Length - 1) ]
    |> List.collect (fun (i, j) ->
        if maze[i][j] = '#' then
            []
        else
            [ E; N; W; S ]
            |> List.collect (fun dir ->
                let node = { Row = i; Col = j; Dir = dir }

                let forward =
                    node.Forward(maze) |> Option.map (fun n -> { From = node; To = n; Cost = 1 })

                let rotate =
                    node.Rotate() |> List.map (fun n -> { From = node; To = n; Cost = 1000 })

                match forward with
                | Some forward -> forward :: rotate
                | None -> rotate))

let findIndex2D (a: 'T[][]) (v: 'T) =
    List.allPairs [ 0 .. (a.Length - 1) ] [ 0 .. (a[0].Length - 1) ]
    |> List.find (fun (i, j) -> a[i][j] = v)

let part1 (maze: char[][]) =
    let edges = collectEdges maze
    let si, sj = findIndex2D maze 'S'
    let gi, gj = findIndex2D maze 'E'

    let start = { Row = si; Col = sj; Dir = E }
    let dist = dijkstra start edges

    [ E; N; W; S ]
    |> List.map (fun dir ->
        let goal = { Row = gi; Col = gj; Dir = dir }
        dist[goal])
    |> List.min

let part2 (maze: char[][]) =
    let edges = collectEdges maze
    let si, sj = findIndex2D maze 'S'
    let gi, gj = findIndex2D maze 'E'

    let start = { Row = si; Col = sj; Dir = E }
    let dist = dijkstra start edges

    let dir =
        [ E; N; W; S ]
        |> List.minBy (fun dir ->
            let goal = { Row = gi; Col = gj; Dir = dir }
            dist[goal])

    let goal = { Row = gi; Col = gj; Dir = dir }
    let revEdges = edges |> List.map (fun e -> { e with From = e.To; To = e.From })
    let revDist = dijkstra goal revEdges

    let minD = part1 maze

    List.allPairs [ 0 .. (maze.Length - 1) ] [ 0 .. (maze[0].Length - 1) ]
    |> List.filter (fun (i, j) ->
        if maze[i][j] = '#' then
            false
        else
            [ E; N; W; S ]
            |> List.exists (fun dir ->
                let node = { Row = i; Col = j; Dir = dir }

                match Map.tryFind node dist, Map.tryFind node revDist with
                | Some d, Some rd when d + rd = minD -> true
                | _ -> false)

    )
    |> List.length

let parse (input: string) =
    input.Split("\n") |> Array.map (fun line -> line.ToCharArray())

module Example =
    let input =
        "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 7036

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 45

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let maze = parse input

    maze |> part1 |> printfn "Part 1: %d"
    maze |> part2 |> printfn "Part 2: %d"

    0
