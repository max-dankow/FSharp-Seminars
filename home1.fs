open System;

let rec private len' acc = function
    | [] -> acc
    | _::xs -> len' (acc + 1) xs
let len l = len' 0 l

let printList =
    List.iter (printf "%A ")

printList [1..100]

let rec qsort = function
    | [] -> []
    | x::xs ->
        let (l, r) = List.partition ((>=) x) xs
        (qsort l) @ (x::qsort r)

//let readInt() = Int32.Parse(System.Console.ReadLine())

//let rec private readIntList' acc = 
//   match readInt() with
//    | 0 -> acc
//    | x -> readIntList' (x::acc)

//let readIL = readIntList' []

//readIL


qsort [9;5;0;8;7;3]

// Решето Эратосфена.
let rec resheto = function
    | [] -> []:int list
    | h::t when h >= 2 -> h::(List.filter(fun x -> x%h<>0) t |> resheto)
    | _::t -> resheto t

resheto [0..100]

// Хвостовая свертка.
let rec foldr f acc = function
    | [] -> acc
    | x::xs -> foldr f (f acc x) xs

foldr (fun acc _ -> acc + 1) 0 [1..100]

let append l1 l2 = List.foldBack (fun x acc -> x::acc) l1 l2

append [1;2;3] [9;8]

let inline average l =
    let average' = List.fold (fun (sum, count) x -> (sum + x, count + 1)) (LanguagePrimitives.GenericZero, 0)
    let (sum, count) = average' l
    LanguagePrimitives.DivideByInt sum count

average [for x in [1..4] -> float(x)]
List.map (float) [1..40] |> average

// Матрица.






// Список всех перестановок.
//let permute l = 

