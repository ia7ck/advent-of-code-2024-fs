open Xunit
open FsUnit.Xunit

let rec region (map: char[][]) (i, j) acc =
    (Set.add (i, j) acc, [ (-1, 0); (0, -1); (1, 0); (0, 1) ])
    ||> List.fold (fun acc (di, dj) ->
        let ni, nj = i + di, j + dj

        if
            0 <= ni
            && ni < map.Length
            && 0 <= nj
            && nj < map[i].Length
            && map[i][j] = map[ni][nj]
            && not (Set.contains (ni, nj) acc)
        then
            region map (ni, nj) acc
        else
            acc)

let solve (map: char[][]) calcPrice =
    let n = map.Length
    map |> Array.iter (fun row -> row.Length |> should equal n)

    (Set.empty, List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ])
    ||> List.mapFold (fun seen (i, j) ->
        if Set.contains (i, j) seen then
            0, seen
        else
            let r = region map (i, j) Set.empty
            calcPrice r, Set.union seen r)
    |> fst
    |> List.sum

let part1 (map: char[][]) =
    let calcPrice positions =
        let area = Seq.length positions

        let perimeter =
            positions
            |> Seq.sumBy (fun (i, j) ->
                [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
                |> List.sumBy (fun (di, dj) ->
                    let ni, nj = i + di, j + dj

                    if
                        0 <= ni
                        && ni < map.Length
                        && 0 <= nj
                        && nj < map[ni].Length
                        && map[i][j] = map[ni][nj]
                    then
                        0
                    else
                        1))

        area * perimeter

    solve map calcPrice

let part2 (map: char[][]) =
    let calcPrice positions =
        let area = Seq.length positions

        let intersections =
            positions
            |> Seq.collect (fun (i, j) -> [ (i, j); (i + 1, j); (i, j + 1); (i + 1, j + 1) ])
            |> Set.ofSeq

        // https://atcoder.jp/contests/abc191/tasks/abc191_c
        let countCorner = // = number of sides
            intersections
            |> Seq.sumBy (fun (i, j) ->
                let surrounding =
                    Set.intersect (set [ (i - 1, j - 1); (i - 1, j); (i, j - 1); (i, j) ]) positions

                let size = Set.count surrounding

                if size = 3 then
                    // ##  #.  .#  ##
                    // #.  ##  ##  .#
                    1
                else if size = 1 then
                    // #.  ..  ..  .#
                    // ..  #.  .#  ..
                    1
                else if surrounding = set [ (i - 1, j - 1); (i, j) ] then
                    // #.
                    // .#
                    2
                else if surrounding = set [ (i - 1, j); (i, j - 1) ] then
                    // .#
                    // #.
                    2
                else
                    0)

        area * countCorner

    solve map calcPrice

let parse (input: string) =
    input.Split("\n") |> Array.map (fun row -> row.ToCharArray())

module Example =
    type Small() =
        let input =
            "AAAA
BBCD
BBCC
EEEC"

        [<Fact>]
        let testPart1 () =
            parse input |> part1 |> should equal 140

        [<Fact>]
        let testPart2 () = parse input |> part2 |> should equal 80

    type OX() =
        let input =
            "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"

        [<Fact>]
        let testPart1 () =
            parse input |> part1 |> should equal 772

        [<Fact>]
        let testPart2 () =
            parse input |> part2 |> should equal 436

    type EX() =
        let input =
            "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"

        [<Fact>]

        let testPart2 () =
            parse input |> part2 |> should equal 236

    type AB() =
        let input =
            "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"

        [<Fact>]

        let testPart2 () =
            parse input |> part2 |> should equal 368

    type Large() =
        let input =
            "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

        [<Fact>]
        let testPart1 () =
            parse input |> part1 |> should equal 1930

        [<Fact>]
        let testPart2 () =
            parse input |> part2 |> should equal 1206

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let map = parse input

    map |> part1 |> printfn "Part 1: %d"
    map |> part2 |> printfn "Part 2: %d"

    0
