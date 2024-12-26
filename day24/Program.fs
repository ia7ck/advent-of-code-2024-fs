open System.Text.RegularExpressions

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

let part2 ((_wires, gates): ((string * int) seq) * Gate seq) =
    let hasLoop gateByOut =
        let rec dfs out path =
            if List.contains out path then
                true
            else
                match Map.tryFind out gateByOut with
                | None -> false
                | Some g ->
                    let left, right = g.Input
                    dfs left (out :: path) || dfs right (out :: path)

        gateByOut |> Map.keys |> Seq.exists (fun out -> dfs out [])

    let rec collect out gateByOut =
        match Map.tryFind out gateByOut with
        | None -> []
        | Some g ->
            let left, right = g.Input
            out :: collect left gateByOut @ collect right gateByOut

    let rec make out gateByOut =
        match Map.tryFind out gateByOut with
        | None -> out
        | Some g ->
            let left, right = make (fst g.Input) gateByOut, make (snd g.Input) gateByOut
            let left, right = min left right, max left right

            match g.Op with
            | And -> $"({left})and({right})"
            | Or -> $"({left})or({right})"
            | Xor -> $"({left})xor({right})"

    let valid out gateByOut =
        let circuit = make out gateByOut

        let validXY =
            // x00, y00, x01, y01, x01, y01, x02, y02, x02, y02, ...
            let xy =
                Regex.Matches(circuit, @"[x|y][0-9]+")
                |> Seq.map (fun m -> m.Value)
                |> Seq.chunkBySize 2

            match List.ofSeq xy with
            | [] -> true
            | [| "x00"; "y00" |] :: t ->
                let t = List.chunkBySize 2 t
                let n = List.length t

                [ 1..n ]
                |> List.forall (fun i ->
                    let xy = [| $"x%02d{i}"; $"y%02d{i}" |]

                    if i < n then
                        t[i - 1] = List.replicate 2 xy
                    else
                        t[i - 1] = [ xy ])
            | _ -> false

        let validOp =
            // ((((((x00)and(y00))and((x01)xor(y01)))or((x01)and(y01)))and((x02)xor(y02)))or((x02)and(y02)))xor((x03)xor(y03))
            // and, and, xor, or, and, and, xor, or, and, xor, xor
            //      ^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^
            let ops = Regex.Matches(circuit, @"(and|or|xor)") |> Seq.map (fun m -> m.Value)

            match List.ofSeq ops with
            | [] -> true
            | [ "xor" ] -> true // i = 0
            | "and" :: t ->
                let t = List.chunkBySize 4 t
                let n = List.length t

                [ 0 .. (n - 1) ]
                |> List.forall (fun i ->
                    if i + 1 < n then
                        t[i] = [ "and"; "xor"; "or"; "and" ]
                    else
                        t[i] = [ "xor"; "xor" ])
            | _ -> false

        validXY && validOp

    let rec search i gateByOut =
        if i >= 45 then
            Some gateByOut
        else
            let out = $"z%02d{i}"

            if valid out gateByOut then
                search (i + 1) gateByOut
            else
                let swaps = collect out gateByOut

                swaps
                |> List.tryPick (fun out ->
                    Map.toList gateByOut
                    |> List.tryPick (fun (out', g') ->
                        let g = gateByOut[out]
                        let newGateByOut = gateByOut |> Map.add out g' |> Map.add out' g

                        if hasLoop newGateByOut then
                            None
                        else
                            let nextInvalid =
                                [ 0..45 ] |> List.find (fun j -> valid $"z%02d{j}" newGateByOut |> not)

                            if i < nextInvalid then
                                search nextInvalid newGateByOut
                            else
                                None))

    let gateByOut =
        gates
        |> Seq.groupBy (fun g -> g.Output)
        |> Seq.map (fun (out, gates) -> out, Seq.exactlyOne gates)
        |> Map

    let correctGateByOut = search 0 gateByOut |> Option.get

    let diff =
        gateByOut
        |> Map.keys
        |> Seq.filter (fun out ->
            let g = gateByOut[out]
            let g' = correctGateByOut[out]
            g <> g')

    diff |> Seq.sort |> String.concat ","

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

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let (wires, gates) = parse input

    (wires, gates) |> part1 |> printfn "Part 1: %d"
    (wires, gates) |> part2 |> printfn "Part 2: %s"

    0
