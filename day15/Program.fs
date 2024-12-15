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

let reverse (a: 'a[][]) = Array.map (Array.rev) a

let transpose (a: 'a[][]) =
    let h, w = a.Length, a[0].Length
    Array.init w (fun i -> Array.init h (fun j -> a[j][i]))

let findRobot (map: Cell[][]) =
    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.find (fun (i, j) -> map[i][j] = Robot)

let moveLeft (map: Cell[][]) =
    let ri, rj = findRobot map

    match map[ri][rj - 1] with
    | Wall -> Some map
    | Empty ->
        let newRow = map[ri] |> Array.updateAt (rj - 1) Robot |> Array.updateAt rj Empty
        map |> Array.updateAt ri newRow |> Some
    | Box ->
        let start = [ 0 .. (rj - 1) ] |> List.findBack (fun j -> map[ri][j] <> Box)

        if map[ri][start] <> Empty then
            None
        else
            // #....OOO@..
            // ↓
            // #...OOO@...
            let newRow =
                map[ri]
                |> Array.mapi (fun j' c ->
                    if start <= j' && j' < rj - 1 then Box
                    else if j' = rj - 1 then Robot
                    else if j' = rj then Empty
                    else c)

            map |> Array.updateAt ri newRow |> Some
    | Robot -> failwith "!?"

let moveRight (map: Cell[][]) =
    map |> reverse |> moveLeft |> Option.map reverse

let moveUp (map: Cell[][]) =
    map |> transpose |> moveLeft |> Option.map transpose

let moveDown (map: Cell[][]) =
    map |> transpose |> moveRight |> Option.map transpose

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

            mv map |> Option.defaultWith (fun () -> map))

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
            | _ -> failwith "!?"))

let parse (input: string) =
    let input = input.Split("\n\n")
    let map, moves = input[0], input[1]

    let map = parseMap map

    let moves =
        moves.Split("\n")
        |> Array.map (fun moves ->
            moves.ToCharArray()
            |> Array.map (function
                | '^' -> U
                | '<' -> L
                | 'v' -> D
                | '>' -> R
                | _ -> failwith "!?"))
        |> Array.concat


    (map, moves)

module Example =
    type MoveTest() =
        [<Fact>]
        let moveLeftTest () =
            parseMap "#..OO@." |> moveLeft |> should equal (Some(parseMap "#.OO@.."))

        [<Fact>]
        let moveRightTest () =
            parseMap "#.@OO.." |> moveRight |> should equal (Some(parseMap "#..@OO."))


        [<Fact>]
        let moveUpTest () =
            parseMap "#..OO@."
            |> transpose
            |> moveUp
            |> should equal (Some(parseMap "#.OO@.." |> transpose))

        [<Fact>]
        let moveUpTest () =
            parseMap "#.@OO.."
            |> transpose
            |> moveDown
            |> should equal (Some(parseMap "#..@OO." |> transpose))

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
