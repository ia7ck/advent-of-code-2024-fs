open Xunit
open FsUnit.Xunit

let getC i j (chars: char[][]) =
    if 0 <= i && i < chars.Length && 0 <= j && j < chars[i].Length then
        Some(chars[i][j])
    else
        None

let part1 (chars: char[][]) =
    let getC i j = getC i j chars

    chars
    |> Array.indexed
    |> Array.sumBy (fun (i, row) ->
        row
        |> Array.indexed
        |> Array.sumBy (fun (j, _) ->
            let north = [ 0..3 ] |> List.map (fun d -> getC (i - d) j)
            let northWest = [ 0..3 ] |> List.map (fun d -> getC (i - d) (j - d))
            let west = [ 0..3 ] |> List.map (fun d -> getC i (j - d))
            let southWest = [ 0..3 ] |> List.map (fun d -> getC (i + d) (j - d))
            let south = [ 0..3 ] |> List.map (fun d -> getC (i + d) j)
            let southEast = [ 0..3 ] |> List.map (fun d -> getC (i + d) (j + d))
            let east = [ 0..3 ] |> List.map (fun d -> getC i (j + d))
            let northEast = [ 0..3 ] |> List.map (fun d -> getC (i - d) (j + d))

            [ north; northWest; west; southWest; south; southEast; east; northEast ]
            |> List.filter (fun word -> word = [ Some('X'); Some('M'); Some('A'); Some('S') ])
            |> List.length))

let part2 (chars: char[][]) =
    let getC i j = getC i j chars

    let isMAS (c1, c2, c3) =
        c1 = Some('M') && c2 = Some('A') && c3 = Some('S')

    chars
    |> Array.indexed
    |> Array.sumBy (fun (i, row) ->
        row
        |> Array.indexed
        |> Array.filter (fun (j, c) ->
            let ne = getC (i - 1) (j + 1)
            let nw = getC (i - 1) (j - 1)
            let se = getC (i + 1) (j + 1)
            let sw = getC (i + 1) (j - 1)

            (isMAS (ne, Some(c), sw) || isMAS (sw, Some(c), ne))
            && (isMAS (nw, Some(c), se) || isMAS (se, Some(c), nw)))
        |> Array.length)

let parse (input: string) =
    input.Split("\n") |> Array.map (fun row -> row.ToCharArray())

module Example =
    let input =
        "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 18

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 9

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let chars = parse input

    chars |> part1 |> printfn "Part 1: %d"
    chars |> part2 |> printfn "Part 2: %d"

    0
