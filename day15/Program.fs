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

module Part2 =
    type Cell2 =
        | Robot
        | BoxL
        | BoxR
        | Wall
        | Empty

    let downgrade =
        function
        | Robot -> Cell.Robot
        | BoxL
        | BoxR -> Box
        | Wall -> Cell.Wall
        | Empty -> Cell.Empty


    let toString (map: Cell2[][]) =
        map
        |> Array.map (fun row ->
            row
            |> Array.map (function
                | Robot -> '@'
                | BoxL -> '['
                | BoxR -> ']'
                | Wall -> '#'
                | Empty -> '.')
            |> System.String)
        |> String.concat "\n"

    let findRobot (map: Cell2[][]) =
        map |> Array.map (fun row -> row |> Array.map downgrade) |> findRobot

    let rec pushLeft (i, j) (map: Cell2[][]) =
        assert (map[i][j] = BoxR)
        assert (map[i][j - 1] = BoxL)

        match map[i][j - 2] with
        | Wall -> None
        | Empty ->
            // #..[].. -> #.[]...
            let newRow = map[i] |> swap (j - 2, j - 1) |> swap (j - 1, j)
            map |> Array.updateAt i newRow |> Some
        | BoxR ->
            pushLeft (i, j - 2) map
            |> Option.map (fun map ->
                assert (map[i][j - 2] = Empty)
                // delegate
                pushLeft (i, j) map |> Option.get)
        | c -> failwithf "%A !?" c

    let rec pushUp (i, j) (map: Cell2[][]) =
        match map[i][j] with
        | BoxL ->
            assert (map[i][j + 1] = BoxR)

            match map[i - 1][j], map[i - 1][j + 1] with
            | Wall, _
            | _, Wall -> None
            | Empty, Empty ->
                let newRow1 = map[i - 1] |> Array.updateAt j BoxL |> Array.updateAt (j + 1) BoxR
                let newRow2 = map[i] |> Array.updateAt j Empty |> Array.updateAt (j + 1) Empty
                map |> Array.updateAt (i - 1) newRow1 |> Array.updateAt i newRow2 |> Some
            // []
            | BoxL, BoxR
            // ].
            | BoxR, Empty ->
                pushUp (i - 1, j) map
                |> Option.map (fun map ->
                    assert (map[i - 1][j] = Empty)
                    assert (map[i - 1][j + 1] = Empty)
                    // delegate
                    pushUp (i, j) map |> Option.get)
            // .[
            | Empty, BoxL ->
                pushUp (i - 1, j + 1) map
                |> Option.map (fun map ->
                    assert (map[i - 1][j] = Empty)
                    assert (map[i - 1][j + 1] = Empty)
                    // delegate
                    pushUp (i, j) map |> Option.get)
            // ][
            | BoxR, BoxL ->
                pushUp (i - 1, j) map
                |> Option.bind (fun map ->
                    assert (map[i - 1][j - 1] = Empty)
                    assert (map[i - 1][j] = Empty)
                    pushUp (i - 1, j + 1) map)
                |> Option.map (fun map ->
                    assert (map[i - 1][j] = Empty)
                    assert (map[i - 1][j + 1] = Empty)
                    // delegate
                    pushUp (i, j) map |> Option.get)
            | c1, c2 -> failwithf "%A %A !?" c1 c2
        | BoxR ->
            // delegate
            pushUp (i, j - 1) map
        | c -> failwithf "%A !?" c

    let rec moveLeft (map: Cell2[][]) =
        let ri, rj = findRobot map

        match map[ri][rj - 1] with
        | Wall -> map
        | Empty ->
            // #...@. -> #..@..
            let newRow = map[ri] |> swap (rj - 1, rj)
            map |> Array.updateAt ri newRow
        | BoxR ->
            map
            |> pushLeft (ri, rj - 1)
            |> Option.map (fun map ->
                assert (map[ri][rj - 1] = Empty)
                // delegate
                moveLeft map)
            |> Option.defaultWith (fun () -> map)
        | c -> failwithf "%A !?" c

    let rec moveUp (map: Cell2[][]) =
        let ri, rj = findRobot map

        match map[ri - 1][rj] with
        | Wall -> map
        | Empty ->
            let newRow1 = map[ri - 1] |> Array.updateAt rj Robot
            let newRow2 = map[ri] |> Array.updateAt rj Empty
            map |> Array.updateAt (ri - 1) newRow1 |> Array.updateAt ri newRow2
        | BoxL
        | BoxR ->
            map
            |> pushUp (ri - 1, rj)
            |> Option.map (fun map ->
                assert (map[ri - 1][rj] = Empty)
                // delegate
                moveUp map)
            |> Option.defaultWith (fun () -> map)
        | c -> failwithf "%A !?" c

    let moveRight (map: Cell2[][]) =
        let reverse (map: Cell2[][]) =
            map
            |> Array.map (fun row ->
                row
                |> Array.rev
                |> Array.map (function
                    | BoxL -> BoxR
                    | BoxR -> BoxL
                    | c -> c))

        map |> reverse |> moveLeft |> reverse

    let moveDown (map: Cell2[][]) = map |> Array.rev |> moveUp |> Array.rev

    let scaleUp (map: Cell[][]) =
        map
        |> Array.map (fun row ->
            row
            |> Array.collect (function
                | Cell.Robot -> [| Robot; Empty |]
                | Box -> [| BoxL; BoxR |]
                | Cell.Wall -> [| Wall; Wall |]
                | Cell.Empty -> [| Empty; Empty |]))

    let part2 ((map, moves): (Cell[][] * Dir seq)) =
        let map =
            (scaleUp map, moves)
            ||> Seq.fold (fun map dir ->
                let mv =
                    match dir with
                    | U -> moveUp
                    | L -> moveLeft
                    | D -> moveDown
                    | R -> moveRight

                mv map)

        List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
        |> List.sumBy (fun (i, j) -> if map[i][j] = BoxL then i * 100 + j else 0)


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

    type MoveTest2() =
        [<Fact>]
        let moveLeftTest () =
            parseMap "#..OO@."
            |> Part2.scaleUp
            // ##....[][]@...
            |> Part2.moveLeft
            |> Part2.toString
            |> should equal "##...[][]@...."

        [<Fact>]
        let moveRightTest () =
            parseMap "#.@OO.."
            |> Part2.scaleUp
            // ##..@.[][]....
            |> Part2.moveRight
            |> Part2.toString
            |> should equal "##...@[][]...."

        [<Fact>]
        let moveUpTest () =
            [ "####"; "...."; "..O."; "..O."; "..@." ]
            |> String.concat "\n"
            |> parseMap
            |> Part2.scaleUp
            // ########
            // ........
            // ....[]..
            // ....[]..
            // ....@...
            |> Part2.moveUp
            |> Part2.toString
            |> should
                equal
                "########
....[]..
....[]..
....@...
........"

        [<Fact>]
        let moveUpTest2 () =
            [ "####"; "...."; ".OO."; "..O@"; "...." ]
            |> String.concat "\n"
            |> parseMap
            |> Part2.scaleUp
            // ########
            // ........
            // ..[][]..
            // ....[]@.
            // ........
            |> Part2.moveLeft
            |> Part2.moveDown
            |> Part2.moveLeft
            // ########
            // ........
            // ..[][]..
            // ...[]...
            // ....@...
            |> Part2.moveUp
            |> Part2.toString
            |> should
                equal
                "########
..[][]..
...[]...
....@...
........"

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

    let inputLarge =
        "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 2028

    [<Fact>]
    let testPart2 () =
        parse inputLarge |> Part2.part2 |> should equal 9021

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let (map, moves) = parse input

    (map, moves) |> part1 |> printfn "Part 1: %d"
    (map, moves) |> Part2.part2 |> printfn "Part 2: %d"

    0
