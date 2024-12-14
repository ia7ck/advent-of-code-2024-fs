open Xunit
open FsUnit.Xunit

type Robot = { P: int * int; V: int * int }

type Quadrant =
    | TopRight
    | TopLeft
    | BottomLeft
    | BottomRight

// x 右
// y 下

let rec move times w h (robot: Robot) =
    if times = 0 then
        robot
    else
        let px, py = robot.P
        let vx, vy = robot.V
        let newP = (w + px + vx) % w, (h + py + vy) % h
        move (times - 1) w h { robot with P = newP }

let part1 ((robots, w, h): (Robot seq * int * int)) =
    let quadratics =
        robots
        |> Seq.map (move 100 w h)
        |> Seq.choose (fun { P = (px, py) } ->
            if px > w / 2 && py < h / 2 then Some TopRight
            else if px < w / 2 && py < h / 2 then Some TopLeft
            else if px < w / 2 && py > h / 2 then Some BottomLeft
            else if px > w / 2 && py > h / 2 then Some BottomRight
            else None)

    (1, Seq.countBy id quadratics) ||> Seq.fold (fun acc (_, count) -> acc * count)

let part2 ((robots, w, h): (Robot seq * int * int)) =
    let rec search elapsed robots =
        if elapsed >= 1000 then
            ()
        else
            let positions = robots |> Seq.map (fun robot -> robot.P) |> Set.ofSeq

            let map =
                Array.init h (fun i -> Array.init w (fun j -> if Set.contains (i, j) positions then '@' else ' '))

            printfn "t = %d" elapsed
            map |> Array.map System.String |> String.concat ("\n") |> printfn "%s"
            printfn "\n\n\n\n\n"

            search (elapsed + 1) (robots |> Seq.map (move 1 w h))

    search 0 robots

    // この周期でアスキーアートっぽいものが見える
    // t = 81, 81 + w, 81 + 2w, 81 + 3w, ...
    // t = 30, 30 + h, 30 + 2h, 30 + 3h, ...

    // 適当な範囲で検索する
    [ 0..1000 ]
    |> List.choose (fun p ->
        // 81 + p * w = 30 + q * h
        let numQ = (81 - 30) + p * w
        if numQ % h = 0 then Some(81 + p * w) else None)
    |> List.head

let parse (input: string) =
    input.Split("\n")
    |> Array.map (fun line -> line.Split(" "))
    |> Array.map (fun line ->
        let p, v = line[0], line[1]
        let p = p.Replace("p=", "").Split(",")
        let v = v.Replace("v=", "").Split(",")

        { P = (int p[0], int p[1])
          V = (int v[0], int v[1]) })

module Example =
    let input =
        "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

    [<Fact>]
    let testPart1 () =
        (parse input, 7, 11) |> part1 |> should equal 12

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let robots = parse input

    (robots, 101, 103) |> part1 |> printfn "Part 1: %d"
    (robots, 101, 103) |> part2 |> printfn "Part 2: %d"

    0
