// Разожение exp(x) в ряд Тейлора.
// Модификация iter для хвостовой рекурсии.
let rec iter f x a b = 
    if a>b
    then x
    else iter f (f a x) (a + 1) b

let myexp x n = 
    let rec myexp' x = iter (fun i (prev, sum) -> let current = prev * x / (float i) in (current, sum + current)) (1., 1.) 1 n
    myexp' x |> snd

iter (fun i _ ->  printfn "n=%d, exp(1) = %f" i (myexp 1. i)) (printfn "") 1 10

// Комбинаторы.
let S x y z = x z (y z)
// x * (x + 1)
let mulNext = S ( * ) ((+)1)

// x^2 + 2*x + 1
let sol2 = S ( * ) ((+)2) >> ((+)1)

// N-ое число Фибоначчи БЕЗ использованием хвостовой рекурсии.
let rec fib n = 
    if n = 0 || n = 1
    then n
    else fib (n - 1) + (fib (n - 2))

// N-ое число Фибоначчи с использованием хвостовой рекурсии.
let tailfib n =
    let second (x,y) = y
    let rec tailfib' = iter (fun _ (prevprev, prev) -> let current = prevprev + prev in (prev, current)) (0, 1) 1 (n - 1)
    if n = 0 || n = 1
    then n
    else tailfib' |> second

tailfib 10

// carry - оператор каррирования фукции двух аргументов.
let tupleSum (a, b) = a + b
let curry f x y = f (x, y)
let cSum = curry tupleSum
//tupleSum 1 2
cSum 1 2
