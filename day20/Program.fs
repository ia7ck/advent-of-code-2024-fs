open System.Diagnostics

open Xunit
open FsUnit.Xunit

let findIndex2D (a: 'T[][]) (v: 'T) =
    List.allPairs [ 0 .. (a.Length - 1) ] [ 0 .. (a[0].Length - 1) ]
    |> List.find (fun (i, j) -> a[i][j] = v)

let bfs (si, sj) (maze: char[][]) =
    let transition (i, j) dist =
        let newNodes, newDist =
            (dist, [ (-1, 0); (0, -1); (1, 0); (0, 1) ])
            ||> List.mapFold (fun dist (di, dj) ->
                let ni, nj = i + di, j + dj

                if
                    0 <= ni
                    && ni < maze.Length
                    && 0 <= nj
                    && nj < maze[ni].Length
                    && maze[ni][nj] <> '#'
                    && not (Map.containsKey (ni, nj) dist)
                then
                    (Some(ni, nj), dist |> Map.add (ni, nj) (dist[(i, j)] + 1))
                else
                    (None, dist))

        (List.choose id newNodes, newDist)

    let rec bfs' nodes dist =
        if List.isEmpty nodes then
            dist
        else
            let newNodes, newDist =
                (dist, nodes) ||> List.mapFold (fun dist node -> transition node dist)

            bfs' (List.concat newNodes) newDist

    bfs' [ (si, sj) ] (Map [ (si, sj), 0 ])

let part1 (maze: char[][]) =
    let si, sj = findIndex2D maze 'S'
    let gi, gj = findIndex2D maze 'E'

    let dist = bfs (si, sj) maze

    List.allPairs [ 0 .. (maze.Length - 1) ] [ 0 .. (maze[0].Length - 1) ]
    |> List.choose (fun (i, j) ->
        let tate =
            i >= 1 && i + 1 < maze.Length && maze[i - 1][j] <> '#' && maze[i + 1][j] <> '#'

        let yoko =
            j >= 1
            && j + 1 < maze[i].Length
            && maze[i][j - 1] <> '#'
            && maze[i][j + 1] <> '#'

        if maze[i][j] = '#' && (tate || yoko) then
            maze[i][j] <- '.'
            let d = bfs (si, sj) maze
            maze[i][j] <- '#'

            Some(dist[(gi, gj)] - d[(gi, gj)])
        else
            None)
    |> List.countBy id
    |> List.sort

let part2 (maze: char[][]) =
    let si, sj = findIndex2D maze 'S'

    let dist = bfs (si, sj) maze

    List.allPairs (Map.toList dist) (Map.toList dist)
    |> List.choose (fun (((i, j), d), ((i', j'), d')) ->
        let e = abs (i - i') + abs (j - j')
        if d' - d >= 0 && e <= 20 then Some(d' - d - e) else None)
    |> List.countBy id
    |> List.sort

let parse (input: string) =
    input.Split("\n") |> Array.map (fun line -> line.ToCharArray())

module Example =
    let input =
        "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

    [<Fact>]
    let testPart1 () =
        parse input
        |> part1
        |> should
            be
            (supersetOf
                [ (2, 14)
                  (4, 14)
                  (6, 2)
                  (8, 4)
                  (10, 2)
                  (12, 3)
                  (20, 1)
                  (36, 1)
                  (38, 1)
                  (40, 1)
                  (64, 1) ])

    [<Fact>]
    let testPart2 () =
        parse input
        |> part2
        |> should
            be
            (supersetOf
                [ (50, 32)
                  (52, 31)
                  (54, 29)
                  (56, 39)
                  (58, 25)
                  (60, 23)
                  (62, 20)
                  (64, 19)
                  (66, 12)
                  (68, 14)
                  (70, 12)
                  (72, 22)
                  (74, 4)
                  (76, 3) ])

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let maze = parse input

    let timer = Stopwatch()
    timer.Start()

    maze
    |> part1
    |> Seq.sumBy (fun (save, ps) -> if save >= 100 then ps else 0)
    |> printfn "Part 1: %d"

    timer.Stop()
    printfn "elapsed = %A" timer.Elapsed // 00:04:17.2126316


    timer.Restart()

    maze
    |> part2
    |> Seq.sumBy (fun (save, ps) -> if save >= 100 then ps else 0)
    |> printfn "Part 2: %d"

    timer.Stop()
    printfn "elapsed = %A" timer.Elapsed // 00:01:08.6627788

    0
