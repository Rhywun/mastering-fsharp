open System.Text.RegularExpressions

let (=~) input pattern = Regex.IsMatch(input, pattern)

let rec fib2 n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | x -> fib2 (x - 2) + fib2 (x - 1)

let rec fib3 = function
    | 0 -> 0
    | 1 -> 1
    | x -> fib3 (x - 2) + fib3 (x - 1)

let rec fib4 = function
    | 0 | 1 as x -> x
    | x -> fib4 (x - 2) + fib4 (x - 1)

let rec fib5 n =
    match n with
    | x when x < 2 -> x
    | x -> fib5 (x - 2) + fib5 (x - 1)

let isEven x =
    match x with
    | i when i % 2 = 0 -> true
    | _                -> false

let compare_xy = function
    | (0, _) -> "x is zero"
    | (_, 0) -> "y is zero"
    | _      -> "x and y are different than zero"

type Employee = {
    firstName   : string
    lastName    : string
    gender      : string
    phoneNumber : string
    salary      : float
}

let emp = { firstName = "Joe"; lastName = "Smith"; gender = "M";
    phoneNumber = "1"; salary = 12.0 }

let dave = { emp with firstName = "Dave"; salary = 14.0 }

type Point = {
    x: int
    y: int
}

let isEmptyPoint p =
    match p with
    | { x = 0; y = 0 } -> true
    | _                -> false

type AlphaNumeric =
    | Alpha of string
    | Numeric of int

let a = Alpha "a"
let n = Numeric 1

type Number =
    | Zero
    | Integer   of int
    | Real      of float

let add t1 t2 =
    match t1, t2 with
    | Zero, n | n, Zero -> n
    | Integer i1, Integer i2 -> Integer (i1 + i2)
    | Integer i, Real r -> Real (r + (float i))
    | Real r, Integer i -> Real (r + (float i))
    | Real r1, Real r2 -> Real (r1 + r2)

let intVal = Some 1
let notInt = None

let isSome x =
    match x with
    | Some(_) -> true
    | None    -> false

// bind - Invokes a function on an optional value that itself yields an option

let s1 = Some "Mirror Image"
let s2 = None
let reverse s =
    match s with
    | "" -> None
    | s' -> Some(new System.String(s.ToCharArray() |> Array.rev))
open Option
let r1 = Option.bind reverse s1
let r2 = Option.bind reverse s2

// Queue

type Queue<'T> = | Q of 'T array ref

let create<'t>() =
    Queue<'t>.Q( ref Array.empty )

let push queue t =
    match queue with
    | Q(x) ->
        let r = !x
        x := Array.append r [|t|]
        Q( x )

let pop queue =
    match queue with
    | Q(x) ->
        let r = !x
        let a = Seq.head r
        x := (Seq.skip 1 r) |> Seq.toArray
        a

// Active patterns

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd
    let testNumber = function
       | Even -> sprintf "%d is even" input
       | Odd -> sprintf "%d is odd" input


    // the code in the book doesn't work - giving up
