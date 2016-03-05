// I means bigINt
111111111111111111111111111111111111111111111111111111111111111111111I

//42 + 3.4

let v = (float) 42 + 3.4

// max is standart function
10. - max 100. v

min 10 (min 2 (min  5 (min -1 99)))

2-2-2 // (2 - 2) - 2

let inc = (+) 1

let inc2  x:float = x + 1.

let rec fac n = 
    if n = 0
    then 1
    else n * fac(n - 1) 

fac 5

// strings UTF-8
let m = "s1"
let p = "s2"
let strsum = m + " " + p

// Lists
let lastNumbs = [4;5;6;7;8;9]
let cards = [3;7;12]
let mix = lastNumbs @ cards
List.concat [lastNumbs; [0;0;0]]

[]
//[1; "asd", 1.]
[[1];[2]]
[(+); (-); (/)]

// :: pair constructor
let fcards = 1 :: cards
//[3]::[1;2;3]
[3]::[[1;2;3]]
let bbb = mix.[3]
let try1 = 13::[]

List.head [1..5]
List.head [13]
//List.head[]
List.length [2]
//List.tail Entire list excluding head elenemt

let rec length x = 
    if List.isEmpty x
    then 0
    else 1 + length (List.tail x)

let rec length2 x = 
    match x with 
    | [] -> 0
    | x -> 1 + length (List.tail x)

let rec take n x = 
    if n = 0
    then List.head x
    else take (n - 1) (List.tail x)

let rec lTake n x = 
    if n = 0
    then []
    else List.head x :: (lTake (n - 1) (List.tail x))
take 3 lastNumbs
lTake 3 [1..10]
