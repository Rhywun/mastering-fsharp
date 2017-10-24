let sqr x = x * x

let rec fib n =
    if n < 2 then 1
    else fib (n - 2) + fib (n - 1)

let z =
    let x = 3
    let y = 4
    x + y

[2; 4; 6] |> List.map (fun x -> x + x) |> List.filter (fun x -> x > 5)

// tail-recursion
let factorial x =
   // Keep track of both x and an accumulator value (acc)
   let rec tailRecursiveFactorial x acc =
       if x <= 1 then
           // use the accumulator that has the final result
           acc
       else
           // pass the accumulator + original value again to the recursive method
           tailRecursiveFactorial (x - 1) (acc * x)
   tailRecursiveFactorial x 1
