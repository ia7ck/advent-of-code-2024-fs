open Xunit
open FsUnit.Xunit

type Instruction =
    | Multiply of int * int
    | Enable
    | Disable

let part1 (multiplies: (int * int) seq) =
    multiplies |> Seq.sumBy (fun (x, y) -> x * y)

let part2 (instructions: Instruction seq) =
    ((0, true), instructions)
    ||> Seq.fold (fun (sum, on) ins ->
        match ins with
        | Multiply(x, y) ->
            let newSum = if on then sum + x * y else sum
            (newSum, on)
        | Enable -> (sum, true)
        | Disable -> (sum, false))
    |> fst

let parseNumber (text: string) =
    let digits = Seq.takeWhile System.Char.IsAsciiDigit text

    if Seq.isEmpty digits then
        None
    else
        let v = System.String.Concat digits
        Some(int v, text[v.Length ..])

let parsePrefix (p: string) (text: string) =
    if text.StartsWith(p) then
        Some(p, text[p.Length ..])
    else
        None

let parseMul (text: string) =
    parsePrefix "mul(" text
    |> Option.bind (fun (_, rest) -> parseNumber rest)
    |> Option.bind (fun (x, rest) -> parsePrefix "," rest |> Option.map (fun (_, rest) -> (x, rest)))
    |> Option.bind (fun (x, rest) -> parseNumber rest |> Option.map (fun (y, rest) -> ((x, y), rest)))
    |> Option.bind (fun ((x, y), rest) -> parsePrefix ")" rest |> Option.map (fun (_, rest) -> ((x, y), rest)))

let parse1 (input: string) =
    let rec p (text: string) acc =
        if text.Length = 0 then
            List.rev acc
        else
            match parseMul text with
            | Some((x, y), rest) -> p rest ((x, y) :: acc)
            | None -> p text[1..] acc

    p input []

let parse2 (input: string) =
    let rec p (text: string) acc =
        if text.Length = 0 then
            List.rev acc
        else
            parseMul text
            |> Option.map (fun ((x, y), rest) -> p rest (Multiply(x, y) :: acc))
            |> Option.orElseWith (fun () ->
                parsePrefix "do()" text |> Option.map (fun (_, rest) -> p rest (Enable :: acc)))
            |> Option.orElseWith (fun () ->
                parsePrefix "don't()" text
                |> Option.map (fun (_, rest) -> p rest (Disable :: acc)))
            |> Option.defaultWith (fun () -> p text[1..] acc)

    p input []

module Example =
    let input1 =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    let input2 =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

    [<Fact>]
    let testParse1 () =
        parse1 input1 |> should equal [ (2, 4); (5, 5); (11, 8); (8, 5) ]

    [<Fact>]
    let testPart1 () =
        parse1 input1 |> part1 |> should equal 161

    [<Fact>]
    let testParse2 () =
        parse2 input2
        |> should
            equal
            [ Multiply(2, 4)
              Disable
              Multiply(5, 5)
              Multiply(11, 8)
              Enable
              Multiply(8, 5) ]

    [<Fact>]
    let testPart2 () =
        parse2 input2 |> part2 |> should equal 48

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()

    parse1 input |> part1 |> printfn "Part 1: %d"
    parse2 input |> part2 |> printfn "Part 2: %d"

    0
