// Hascell - лень
// F# - эффектвный
// Seq - генератор
//Seq.take 4 [1..10]
//Seq.take 4 (Seq.initInfinite(fun i -> i + 1))

// Take с обработкой граничных случаев.
let rec takeL n l =
    if n > List.length l
    then l
    else 
        if n <= 0
        then []
        else (List.head l)::(takeL (n - 1) (List.tail l))

let rec take n l =
    let len = List.length l in
    if n >= len || n < 0
    then None
    else 
        if n = 0
        then Some(List.head l)
        else take (n - 1) (List.tail l)
          
takeL -100 [1..10]
take 100 [0..100]
match  take 101 [0..100] with
| None -> printfn "Netu"
| Some(x) -> printfn "Est' = %d" x

List.min [1..100]

let rec product l = 
    match l with
    | [] -> 1
    | h::t -> h * (product t)

product [10; 3]