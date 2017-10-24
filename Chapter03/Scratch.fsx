// Sequences

let intSeqs =
    seq {
        for i = 0 to 10 do
            yield i * 2
    }

let intSeqs2 =
    seq {
        for i = 0 to 10 do
            if i % 2 = 0 then
                yield i * 2
    }

let printSeq t = Seq.iter (printf "%A ") t
let seqMultiplesOfFive = Seq.init 5 (fun i -> i * 5)
let values = Seq.unfold (fun state ->
    if state > 10 then None else Some (state, 2 + state)) 0
let filteredValues = Seq.filter (fun x -> x % 4 = 0) values

seq { 1 .. 5 } |> Seq.filter (fun i -> i > 3)
seq { 1 .. 5 } |> Seq.find (fun i -> i > 3)
seq { 1 .. 5 } |> Seq.tryFind (fun i -> i > 3)

seq { 1 .. 5 } |> Seq.find (fun i -> i > 7)         // Error
seq { 1 .. 5 } |> Seq.tryFind (fun i -> i > 7)      // Returns None

Seq.truncate 2 values
Seq.take 2 values

seq { 1 .. 5 } |> Seq.map (fun i -> 'A' + char i) |> printSeq
seq { 1 .. 10 } |>
    Seq.choose (fun i -> if i % 2 = 0 then Some('A' + char i) else None) |> printSeq

Seq.pairwise values
Seq.windowed 3 values

seq { 1..5 } |> Seq.fold (fun state i -> state + i) 5
seq { 1..5 } |> Seq.fold (+) 5

seq { 1..5 } |> Seq.reduce (+)
[5;2;7;4;3] |> Seq.sort |> printSeq
[1..5] |> Seq.sortBy (fun i -> -i) |> printSeq

[1..20] |> Seq.groupBy(fun i -> if i % 2 = 0 then 0 else 1) |> printSeq

// Arrays

open Array

let a1 : int[] = Array.zeroCreate 10                    // Type spec is required here
let a2 = Array.create 10 3
let a3 = Array.init 10 (fun x -> x / 2)

// Lists

[1;2;3].Head                    // Property
List.head [1;2;3]               // Function

let rec sumList l =
    match l with
    | [] -> 0
    | h :: t -> h + sumList t

[1;2;3] |> List.zip [4;5;6]

// Sets

Set.empty.Add(1).Add(20).Add(1)

// Maps

Map.empty.Add("1", "One").Add("2", "Two")
Map.empty.Add("1", "One").Add("2", "Two").["1"]
