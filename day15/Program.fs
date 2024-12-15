open Xunit
open FsUnit.Xunit

type Cell =
    | Robot
    | Box
    | Wall
    | Empty

type Dir =
    | U
    | L
    | D
    | R

let swap (i, j) (a: 'T[]) =
    let x, y = a[i], a[j]
    a |> Array.updateAt i y |> Array.updateAt j x

let transpose (a: 'T[][]) =
    let h, w = a.Length, a[0].Length
    Array.init w (fun i -> Array.init h (fun j -> a[j][i]))

let findRobot (map: Cell[][]) =
    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.find (fun (i, j) -> map[i][j] = Robot)

let rec pushLeft (i, j) (map: Cell[][]) =
    assert (map[i][j] = Box)

    match map[i][j - 1] with
    | Wall -> None
    | Empty ->
        // #..OOO. -> # .O.OO.
        let newRow = map[i] |> swap (j - 1, j)
        map |> Array.updateAt i newRow |> Some
    | Box ->
        pushLeft (i, j - 1) map
        |> Option.map (fun map ->
            assert (map[i][j - 1] = Empty)
            // delegate
            pushLeft (i, j) map |> Option.get)
    | Robot -> failwith "!?"

let rec moveLeft (map: Cell[][]) =
    let ri, rj = findRobot map

    match map[ri][rj - 1] with
    | Wall -> map
    | Empty ->
        let newRow = map[ri] |> swap (rj - 1, rj)
        map |> Array.updateAt ri newRow
    | Box ->
        map
        |> pushLeft (ri, rj - 1)
        |> Option.map (fun map ->
            assert (map[ri][rj - 1] = Empty)
            // delegate
            moveLeft map)
        |> Option.defaultWith (fun () -> map)
    | Robot -> failwith "!?"

let moveRight (map: Cell[][]) =
    let reverse (a: 'T[][]) = Array.map (Array.rev) a

    map |> reverse |> moveLeft |> reverse

let moveUp (map: Cell[][]) =
    map |> transpose |> moveLeft |> transpose

let moveDown (map: Cell[][]) =
    map |> transpose |> moveRight |> transpose

let part1 ((map, moves): (Cell[][] * Dir seq)) =
    let map =
        (map, moves)
        ||> Seq.fold (fun map dir ->
            let mv =
                match dir with
                | U -> moveUp
                | L -> moveLeft
                | D -> moveDown
                | R -> moveRight

            mv map)

    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.sumBy (fun (i, j) -> if map[i][j] = Box then 100 * i + j else 0)

let parseMap (input: string) =
    input.Split("\n")
    |> Array.map (fun row ->
        row.ToCharArray()
        |> Array.map (function
            | '@' -> Robot
            | 'O' -> Box
            | '#' -> Wall
            | '.' -> Empty
            | c -> failwith $"{c} !?"))

let parse (input: string) =
    let input = input.Split("\n\n")
    let map, moves = input[0], input[1]

    let map = parseMap map

    let moves =
        moves.Split("\n")
        |> Array.collect (fun moves ->
            moves.ToCharArray()
            |> Array.map (function
                | '^' -> U
                | '<' -> L
                | 'v' -> D
                | '>' -> R
                | c -> failwith $"{c} !?"))


    (map, moves)

module Example =
    type MoveTest() =
        [<Fact>]
        let moveLeftTest () =
            parseMap "#..OO@." |> moveLeft |> should equal (parseMap "#.OO@..")

        [<Fact>]
        let moveRightTest () =
            parseMap "#.@OO.." |> moveRight |> should equal (parseMap "#..@OO.")


        [<Fact>]
        let moveUpTest () =
            parseMap "#..OO@."
            |> transpose
            |> moveUp
            |> should equal (parseMap "#.OO@.." |> transpose)

        [<Fact>]
        let moveUpTest () =
            parseMap "#.@OO.."
            |> transpose
            |> moveDown
            |> should equal (parseMap "#..@OO." |> transpose)

    let input =
        "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 2028

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let (map, moves) = parse input

    (map, moves) |> part1 |> printfn "Part 1: %d"

    0
