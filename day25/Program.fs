open Xunit
open FsUnit.Xunit

let part1 (schematics: char[][] list) =
    let h, w = schematics[0].Length, (schematics[0][0]).Length

    let locks =
        schematics
        |> List.filter (fun s -> s[0] |> Array.forall ((=) '#'))
        |> List.map (fun s ->
            [ 0 .. (w - 1) ]
            |> List.map (fun j -> [ 0 .. (h - 1) ] |> List.sumBy (fun i -> if s[i][j] = '.' then 0 else 1)))

    let keys =
        schematics
        |> List.filter (fun s -> s[0] |> Array.forall ((=) '.'))
        |> List.map (fun s ->
            [ 0 .. (w - 1) ]
            |> List.map (fun j -> [ 0 .. (h - 1) ] |> List.sumBy (fun i -> if s[i][j] = '.' then 0 else 1)))

    List.allPairs locks keys
    |> List.filter (fun (lock, key) -> List.zip lock key |> List.forall (fun (lock, key) -> lock + key <= h))
    |> List.length

let parse (input: string) =
    input.Split("\n\n")
    |> Array.map (fun section -> section.Split("\n") |> Array.map (fun line -> line.ToCharArray()))
    |> Array.toList

module Example =
    let input =
        "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 3

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let schematics = parse input

    schematics |> part1 |> printfn "Part 1: %d"

    0
