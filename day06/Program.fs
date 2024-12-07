open Xunit
open FsUnit.Xunit

type Dir =
    | U
    | R
    | D
    | L

    member this.Next() =
        match this with
        | U -> R
        | R -> D
        | D -> L
        | L -> U

let part1 (map: char[][]) =
    let n = map.Length
    map |> Array.iter (fun row -> row.Length |> should equal n)

    let rec forward (i, j) dir history =
        match dir with
        | U -> if i >= 1 then Some(i - 1, j) else None
        | R -> if j + 1 < n then Some(i, j + 1) else None
        | D -> if i + 1 < n then Some(i + 1, j) else None
        | L -> if j >= 1 then Some(i, j - 1) else None
        |> Option.map (fun (ni, nj) ->
            let ni, nj, nd, nh =
                if map[ni][nj] = '#' then
                    (i, j, dir.Next(), history)
                else
                    (ni, nj, dir, (ni, nj) :: history)

            forward (ni, nj) nd nh)
        |> Option.defaultWith (fun () -> history |> List.distinct |> List.length)

    let si, sj =
        List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
        |> List.find (fun (i, j) -> map[i][j] = '^')

    forward (si, sj) U [ (si, sj) ]

let part2 (map: char[][]) =
    let n = map.Length
    map |> Array.iter (fun row -> row.Length |> should equal n)

    let rec findLoop (map: char[][]) (i, j) dir history =
        match dir with
        | U -> if i >= 1 then Some(i - 1, j) else None
        | R -> if j + 1 < n then Some(i, j + 1) else None
        | D -> if i + 1 < n then Some(i + 1, j) else None
        | L -> if j >= 1 then Some(i, j - 1) else None
        |> Option.map (fun (ni, nj) ->
            let ni, nj, nd =
                if map[ni][nj] = '#' then
                    (i, j, dir.Next())
                else
                    (ni, nj, dir)

            if Set.contains (ni, nj, nd) history then
                true
            else
                findLoop map (ni, nj) nd (Set.add (ni, nj, nd) history))
        |> Option.defaultValue false

    let si, sj =
        List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
        |> List.find (fun (i, j) -> map[i][j] = '^')

    List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
    |> List.filter (fun (i, j) ->
        if map[i][j] = '.' then
            let row = map[i] |> Array.updateAt j '#'
            let newMap = map |> Array.updateAt i row
            findLoop newMap (si, sj) U (set [ (si, sj, U) ])
        else
            false)
    |> List.length

let parse (input: string) =
    input.Split("\n") |> Array.map (fun row -> row.ToCharArray())

module Example =
    let input =
        "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 41

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 6

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let map = parse input

    map |> part1 |> printfn "Part 1: %d"
    map |> part2 |> printfn "Part 2: %d"

    0
