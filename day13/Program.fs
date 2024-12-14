open System.Text.RegularExpressions
open Xunit
open FsUnit.Xunit

type Machine =
    { ButtonA: Button
      ButtonB: Button
      Prize: Prize }

and Button = { AddX: int; AddY: int }
and Prize = { X: int; Y: int }

let tryMin list' =
    if List.isEmpty list' then None else Some(List.min list')

let part1 (machines: Machine seq) =
    machines
    |> Seq.sumBy
        (fun
            { ButtonA = buttonA
              ButtonB = buttonB
              Prize = prize } ->
            List.allPairs [ 0..100 ] [ 0..100 ]
            |> List.filter (fun (i, j) ->
                let x = buttonA.AddX * i + buttonB.AddX * j
                let y = buttonA.AddY * i + buttonB.AddY * j
                x = prize.X && y = prize.Y)
            |> List.map (fun (i, j) -> i * 3 + j)
            |> tryMin
            |> Option.defaultValue 0)

let part2 (machines: Machine seq) =
    machines
    |> Seq.sumBy
        (fun
            { ButtonA = { AddX = ax; AddY = ay }
              ButtonB = { AddX = bx; AddY = by }
              Prize = { X = px; Y = py } } ->
            let px, py = int64 px + 10000000000000L, int64 py + 10000000000000L

            // a[x] * i + b[x] * j = p[x]
            // a[y] * i + b[y] * j = p[y]

            let det = int64 (ax * by - bx * ay)

            if det = 0L then
                0L
            else
                // i = (b[y] * p[x] - b[x] * p[y]) / det
                // j = (-a[y] * p[x] + a[x] * p[y]) / det

                let numI = int64 by * px - int64 bx * py
                let numJ = int64 -ay * px + int64 ax * py

                if numI % det = 0L && numJ % det = 0L then
                    numI / det * 3L + numJ / det
                else
                    0)

let parse (input: string) =
    input.Split("\n\n")
    |> Array.map (fun section -> section.Split("\n"))
    |> Array.map (fun section ->
        let buttonA, buttonB, prize = section[0], section[1], section[2]

        let buttonA =
            let groups =
                Regex.Match(buttonA, @"^Button A: X\+([0-9]+), Y\+([0-9]+)$").Groups
                |> Array.ofSeq

            { AddX = int groups[1].Value
              AddY = int groups[2].Value }

        let buttonB =
            let groups =
                Regex.Match(buttonB, @"^Button B: X\+([0-9]+), Y\+([0-9]+)$").Groups
                |> Array.ofSeq

            { AddX = int groups[1].Value
              AddY = int groups[2].Value }

        let prize =
            let groups =
                Regex.Match(prize, @"^Prize: X=([0-9]+), Y=([0-9]+)$").Groups |> Array.ofSeq

            { X = int groups[1].Value
              Y = int groups[2].Value }

        { ButtonA = buttonA
          ButtonB = buttonB
          Prize = prize })

module Example =
    let input =
        "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 480

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let machines = parse input

    machines |> part1 |> printfn "Part 1: %d"
    machines |> part2 |> printfn "Part 2: %d"

    0
