open Xunit
open FsUnit.Xunit

// 0 adv: A <- A / 2^■
// 1 bxl: B <- B xor ■
// 2 bst: B <- ■ % 8
// 3 jnz: A ≠ 0 ⇒ ip <- ■
// 4 bxc: B <- B xor C
// 5 out: ■ % 8
// 6 bdv: B <- A / 2^■
// 7 cdv: C <- A / 2^■

type Register = { A: int64; B: int64; C: int64 }

type Env =
    { Ip: int
      Register: Register
      Program: int[]
      Out: int64 list }

let rec pow' x n =
    if n = 0L then 1L
    else if n % 2L = 0L then pow' (x * x) (n / 2L)
    else x * pow' x (n - 1L)

let execute register program halt =
    let rec f env =
        match halt env with
        | Some out -> out
        | None ->
            let { Ip = ip
                  Register = reg
                  Program = program
                  Out = out } =
                env

            let combo operand =
                match operand with
                | 0
                | 1
                | 2
                | 3 as x -> int64 x
                | 4 -> reg.A
                | 5 -> reg.B
                | 6 -> reg.C
                | x -> failwith $"{x} !?"

            let opcode = program[ip]
            let operand = program[ip + 1]

            let newEnv =
                match opcode with
                | 0 ->
                    { env with
                        Ip = ip + 2
                        Register =
                            { reg with
                                A = reg.A / (pow' 2 (combo operand)) } }
                | 1 ->
                    { env with
                        Ip = ip + 2
                        Register = { reg with B = reg.B ^^^ operand } }
                | 2 ->
                    { env with
                        Ip = ip + 2
                        Register = { reg with B = (combo operand) % 8L } }
                | 3 ->
                    if reg.A = 0 then
                        { env with Ip = ip + 2 }
                    else
                        { env with Ip = operand }
                | 4 ->
                    { env with
                        Ip = ip + 2
                        Register = { reg with B = reg.B ^^^ reg.C } }
                | 5 ->
                    { env with
                        Ip = ip + 2
                        Out = (combo operand) % 8L :: out }
                | 6 ->
                    { env with
                        Ip = ip + 2
                        Register =
                            { reg with
                                B = reg.A / (pow' 2 (combo operand)) } }
                | 7 ->
                    { env with
                        Ip = ip + 2
                        Register =
                            { reg with
                                C = reg.A / (pow' 2 (combo operand)) } }
                | x -> failwith $"{x} !?"

            f newEnv

    f
        { Ip = 0
          Register = register
          Program = program
          Out = [] }

let outputOnHalt env =
    if env.Ip >= env.Program.Length then
        env.Out |> Array.ofList |> Array.map int |> Array.rev |> Some
    else
        None

let part1 (register: Register) (program: int[]) =
    let out = execute register program outputOnHalt
    out |> Array.map string |> String.concat ","

let part2 (register: Register) (program: int[]) =
    // reversing
    //
    // do
    //   B <- A % 8
    //   B <- B xor 5
    //   C <- A / (2 ** B)
    //   B <- B xor 6
    //   A <- A / (2 ** 3)
    //   B <- B xor C
    //   output(B % 8)
    // while (A <> 0)
    //

    let rec find i a =
        if i = 0 then
            Some a
        else
            [ 0..7 ]
            |> List.tryPick (fun j ->
                let a = a * 8L + int64 j
                let out = execute { register with A = a } program outputOnHalt

                if out = program[(i - 1) ..] then find (i - 1) a else None)

    find program.Length 0 |> Option.get

let parse (input: string) =
    let input = input.Split("\n\n")
    let register = input[0].Split("\n")

    let register =
        { A = register[0].Replace("Register A: ", "") |> int64
          B = register[1].Replace("Register B: ", "") |> int64
          C = register[2].Replace("Register C: ", "") |> int64 }

    let program = input[1].Replace("Program: ", "").Split(",") |> Array.map int

    register, program

module Example =
    let input =
        "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"

    [<Fact>]
    let testPart1 () =
        parse input ||> part1 |> should equal "4,6,3,5,6,3,5,2,1,0"

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let register, program = parse input

    (register, program) ||> part1 |> printfn "Part 1: %s"
    (register, program) ||> part2 |> printfn "Part 2: %d"

    0
