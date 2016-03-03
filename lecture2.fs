
// Inline funtictions.
let inline summ a b = a + b

// Recursive list.
type intList =
    | Nill
    | Cons of int * intList

let l = Cons(1, Cons(2, Nill))

//type t' myList =
//    | Nill
//    | Cons of int * t' myList
let l = 3::5::7::[]
let rec fold f i = function
    | [] -> i
    | h::t -> f h (fold f i t)

let listSum = fold (+) 0
let prod = fold ( * ) 1

let rec map f = function
    | [] -> []
    | h::t -> f h :: map f t
 
map (( * )10) l

let rec qsort = function
    | [] -> []
    | h::t -> 
        //let l = List.filter ((>)h) t
        //let r = List.filter ((<=)h) t
        let (l, r) = List.partition((>) h) t
        (qsort l) @ (h :: (qsort r))
// append - дорогая операция, :: дешевая
qsort [5; 3; 7; 2; 9; 2; 7; 5; 0; 10]