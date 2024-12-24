open Xunit
open FsUnit.Xunit

type Op =
    | And
    | Or
    | Xor

type Gate =
    { Input: string * string
      Op: Op
      Output: string }

let part1 ((wires, gates): ((string * int) seq) * Gate seq) =
    let rec run eval (gates: Gate list) =
        if List.isEmpty gates then
            eval
        else
            ((eval, []), gates)
            ||> List.fold (fun (eval, gates) g ->
                let input1, input2 = g.Input

                if Map.containsKey input1 eval && Map.containsKey input2 eval then
                    let out =
                        match g.Op with
                        | And -> eval[input1] &&& eval[input2]
                        | Or -> eval[input1] ||| eval[input2]
                        | Xor -> eval[input1] ^^^ eval[input2]

                    (Map.add g.Output out eval, gates)
                else
                    (eval, g :: gates))
            ||> run

    let eval = run (Map.ofSeq wires) (List.ofSeq gates)

    let z =
        eval
        |> Map.filter (fun k _ -> k.StartsWith("z"))
        |> Map.values
        |> Seq.rev
        |> Seq.map string
        |> String.concat ""

    System.Convert.ToInt64(z, 2)

let part2 ((wires, gates): ((string * int) seq) * Gate seq) = 0

let parse (input: string) =
    let sections = input.Split("\n\n")

    let wires =
        sections[0].Split("\n")
        |> Array.map (fun line ->
            let line = line.Split(": ")
            (line[0], int line[1]))

    let gates =
        sections[1].Split("\n")
        |> Array.map (fun line ->
            let line = line.Split(" ")
            let input = (line[0], line[2])

            let op =
                match line[1] with
                | "AND" -> And
                | "OR" -> Or
                | "XOR" -> Xor
                | op -> failwithf "%s !?" op

            let output = line[4]

            { Input = input
              Op = op
              Output = output })

    (wires, gates)

module Example =
    let input =
        "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 2024L

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 0

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let (wires, gates) = parse input

    (wires, gates) |> part1 |> printfn "Part 1: %d"
    (wires, gates) |> part2 |> printfn "Part 2: %d"

    0
