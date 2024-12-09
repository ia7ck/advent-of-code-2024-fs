open System.Diagnostics

open Xunit
open FsUnit.Xunit

type Block =
    | Free
    | Occupied of int // id

let checksum (disk: Block[]) =
    disk
    |> Array.indexed
    |> Array.sumBy (fun (i, b) ->
        match b with
        | Free -> 0L
        | Occupied id -> (int64 i) * (int64 id))


let part1 (disk: Block[]) =
    let rec compact l r (disk: Block[]) =
        if l >= r then
            disk
        else
            match (disk[l], disk[r]) with
            | (Free, Free) -> compact l (r - 1) disk
            | (Free, Occupied id) ->
                // swap disk[l], disk[r]
                let newDisk = disk |> Array.updateAt l (Occupied id) |> Array.updateAt r Free
                compact (l + 1) (r - 1) newDisk
            | (Occupied _, Free) -> compact (l + 1) (r - 1) disk
            | (Occupied _, Occupied _) -> compact (l + 1) r disk

    disk |> compact 0 (disk.Length - 1) |> checksum

let part2 (disk: Block[]) =
    let rec compact r (disk: Block[]) =
        if r <= 0 then
            disk
        else
            match disk[r] with
            | Free -> compact (r - 1) disk
            | Occupied id ->
                let length =
                    disk[..r] |> Array.rev |> Array.takeWhile ((=) (Occupied id)) |> Array.length

                let newR = r - length

                let l =
                    [ 0..r ]
                    |> List.tryFind (fun l -> disk[l .. (l + length - 1)] |> Array.forall ((=) Free))

                match l with
                | None -> compact newR disk
                | Some l ->
                    (disk, [ 0 .. (length - 1) ])
                    ||> List.fold (fun acc d ->
                        // swap acc[l + d], acc[newR + 1 + d]
                        acc
                        |> Array.updateAt (l + d) (Occupied id)
                        |> Array.updateAt (newR + 1 + d) Free)
                    |> compact newR

    disk |> compact (disk.Length - 1) |> checksum

let parse (input: string) =
    input.ToCharArray()
    |> Array.map (fun c -> int c - int '0')
    |> Array.indexed
    |> Array.collect (fun (i, d) ->
        let b = if i % 2 = 0 then Occupied(i / 2) else Free
        Array.replicate d b)

module Example =
    let input = "2333133121414131402"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 1928L

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 2858L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let disk = parse input

    disk |> part1 |> printfn "Part 1: %d"

    let timer = new Stopwatch()
    timer.Start()
    disk |> part2 |> printfn "Part 2: %d"
    timer.Stop()
    printfn "elapsed = %A" timer.Elapsed // 00:00:39.5809486

    0
