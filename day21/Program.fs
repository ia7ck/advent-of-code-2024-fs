open Xunit
open FsUnit.Xunit

// +---+---+---+
// | 7 | 8 | 9 |
// +---+---+---+
// | 4 | 5 | 6 |
// +---+---+---+
// | 1 | 2 | 3 |
// +---+---+---+
//     | 0 | A |
//     +---+---+
//       ^
//       |
//
//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
//       ^
//       |
//
//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+
//       ^
//       |
//
//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+


let numPos button =
    match button with
    | '7' -> (0, 0)
    | '8' -> (0, 1)
    | '9' -> (0, 2)
    | '4' -> (1, 0)
    | '5' -> (1, 1)
    | '6' -> (1, 2)
    | '1' -> (2, 0)
    | '2' -> (2, 1)
    | '3' -> (2, 2)
    | '0' -> (3, 1)
    | 'A' -> (3, 2)
    | c -> failwithf "%c !?" c

let dirPos button =
    match button with
    | '^' -> (0, 1)
    | 'A' -> (0, 2)
    | '<' -> (1, 0)
    | 'v' -> (1, 1)
    | '>' -> (1, 2)
    | c -> failwithf "%c !?" c


let numRoute num1 num2 =
    let xi, xj = numPos num1
    let yi, yj = numPos num2
    let di = List.init (abs (xi - yi)) (fun _ -> if xi < yi then 'v' else '^')
    let dj = List.init (abs (xj - yj)) (fun _ -> if xj < yj then '>' else '<')
    let tate = di @ dj @ [ 'A' ]
    let yoko = dj @ di @ [ 'A' ]

    let bottom = [ '0'; 'A' ]
    let left = [ '7'; '4'; '1' ]

    match num1, num2 with
    | num1, num2 when List.contains (num1, num2) (List.allPairs bottom left) -> [ tate ]
    | num1, num2 when List.contains (num1, num2) (List.allPairs left bottom) -> [ yoko ]
    | _ -> List.distinct [ tate; yoko ]

let dirRoute dir1 dir2 =
    let xi, xj = dirPos dir1
    let yi, yj = dirPos dir2
    let di = List.init (abs (xi - yi)) (fun _ -> if xi < yi then 'v' else '^')
    let dj = List.init (abs (xj - yj)) (fun _ -> if xj < yj then '>' else '<')
    let tate = di @ dj @ [ 'A' ]
    let yoko = dj @ di @ [ 'A' ]

    let top = [ '^'; 'A' ]
    let left = [ '<' ]

    match dir1, dir2 with
    | dir1, dir2 when List.contains (dir1, dir2) (List.allPairs top left) -> [ tate ]
    | dir1, dir2 when List.contains (dir1, dir2) (List.allPairs left top) -> [ yoko ]
    | _ -> List.distinct [ tate; yoko ]

let rec minCostPath level b1 b2 =
    if level = 0 then
        [ b2 ]
    else
        let numKeypad = '0' <= b1 && b1 <= '9' || '0' <= b2 && b2 <= '9'
        let route = if numKeypad then numRoute b1 b2 else dirRoute b1 b2

        route
        |> List.map (fun route ->
            'A' :: route
            |> List.pairwise
            |> List.collect (fun (x, y) -> minCostPath (level - 1) x y))
        |> List.minBy List.length

let memo = System.Collections.Generic.Dictionary()

// いま b1 にいて b2 を押すために必要な最小手数
let rec minCost level b1 b2 =
    let key = (level, b1, b2)

    match memo.TryGetValue(key) with
    | true, value -> value
    | false, _ ->
        let value =
            if level = 0 then
                1L
            else
                let numKeypad = '0' <= b1 && b1 <= '9' || '0' <= b2 && b2 <= '9'
                let route = if numKeypad then numRoute b1 b2 else dirRoute b1 b2

                route
                |> List.map (fun route ->
                    // level-1 では最初 'A' にいるはず
                    'A' :: route
                    |> List.pairwise
                    |> List.sumBy (fun (x, y) -> minCost (level - 1) x y))
                |> List.min

        memo.Add(key, value)
        value

let part1 (codes: string seq) =
    codes
    |> Seq.sumBy (fun code ->
        let cost =
            'A' :: List.ofSeq code
            |> List.pairwise
            |> List.collect (fun (x, y) -> minCostPath (2 + 1) x y)
            |> List.length
            |> int64

        let num = code.TrimEnd('A') |> int64
        cost * num)

let part2 (codes: string seq) =
    codes
    |> Seq.sumBy (fun code ->
        let cost =
            'A' :: List.ofSeq code
            |> List.pairwise
            |> List.sumBy (fun (x, y) -> minCost (25 + 1) x y)

        let num = code.TrimEnd('A') |> int64
        cost * num)


let parse (input: string) = input.Split("\n")

module Example =
    let input =
        "029A
980A
179A
456A
379A"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 126384L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let codes = parse input

    codes |> part1 |> printfn "Part 1: %d"
    codes |> part2 |> printfn "Part 2: %d"

    0
