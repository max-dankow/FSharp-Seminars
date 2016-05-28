// Деревья.
type 'T Tree =
| Leaf of 'T
| Node of 'T * ('T Tree list)

open System.IO

let dir = "/home/max_dankow/Downloads"
let dirArray = Directory.GetDirectories dir
let fileArray = Directory.GetFiles dir

let rec du dir = 
    let size = Directory.GetFiles dir 
                |> Array.map (fun f -> new FileInfo(f)) 
                |> Array.fold (fun acc x -> acc + x.Length) 0L in
    printf "%10d %s\n" size dir
    Array.iter (du) (Directory.GetDirectories dir)

du dir

// Двоичное дерево.
type 'T BinTree = 
| Nil
| Node of 'T * 'T BinTree * 'T BinTree

let Leaf x = Node(x, Nil, Nil)

let expr = Node('+', Node('*', Leaf('1'), Leaf('2')), Leaf('3'))

let prefix x l r = x(); l(); r()
let infix x l r = l(); x(); r()
let postfix x l r = l(); r(); x()

let rec iter trav f = function
    | Nil -> ()
    | Node(x, L, R) -> 
        trav (fun () -> f x) 
             (fun () -> iter trav f L) 
             (fun () -> iter trav f R)

iter prefix (printf "%c") expr
iter infix (printf "%c") expr
iter postfix (printf "%c") expr

// Свертка с отложенными вычислениями.
let rec foldr f i = function
    | Nil -> i
    | Node(x, L, R) ->
        let cur = foldr f i L in
        foldr f (f cur x) R

//let rec foldl f = 
//    let rec fold' acc = function
//        | Nil -> acc
//        | Node(x, L, R) -> fold' (f (fold' acc L) x) R
//    fold'

//let concatTree1 = foldr (fun acc x -> acc + string x) "" expr
//let concatTree2 = foldl (fun acc x -> acc + string x) "" expr

// Деревья поиска.
// Частотный словарь файла.
open System.IO
let file name = File.ReadAllText name

let f = file @"/home/max_dankow/lotr.txt"
let words = f.Split([|' '; ',';'.';';';'-'; '!'; '?'; '(';'(';')';']';'[';'\n'|]) |> Array.filter (fun x -> x.Length>3)

let rec insert x = function
    | Nil -> Leaf((x, 1))
    | Node((w, c), L, R) ->
        if  x = w 
        then Node((w, c + 1), L, R)
        else if x < w
             then Node((w, c), insert x L, R)
             else Node((w, c), L, insert x R)

let dict = Array.fold (fun acc x -> insert x acc) Nil words

let lw = foldr (fun acc x -> x::acc) [] dict

let mostPopular dict n = (List.sortBy (fun (x, c) -> -c) dict) |> List.take n

let mostLOTR = mostPopular lw

mostLOTR 100

// Map - mutable hash table key->value.
words
|> Array.fold (fun (ht:Map<string, int>) x ->
               if Map.containsKey x ht
               then Map.add x ((Map.find x ht) + 1) ht
               else Map.add x 1 ht) Map.empty
|> Map.toList 
|> List.sortBy (fun (x, n) -> -n)
|> List.take 10

let fileTopN name n = 
    (file name).Split([|' '; ',';'.';';';'-'; '!'; '?'; '(';'(';')';']';'[';'\n'|])
    |> Array.filter (fun x -> x.Length > 3)
    |> Array.fold (fun acc x -> insert x acc) Nil
    |> foldr (fun acc x -> x::acc) []
    |> List.sortBy (fun (x, c) -> -c)
    |> List.take n

fileTopN "/home/max_dankow/programming/GeomLib/Geom.h" 10

// Задание - сделать тоже, только с использованием списковых операций.


// zipper 
// Лекция.

let evenPos = [1..10]
            |> List.mapi (fun i x -> (i, x))
            |> List.filter (fun (i, x) -> i%2=0)
            |> List.map (fun (i, x) -> x)

let rec ins x = function
    | [] -> [[x]]
    | h::t ->
        let l = ins x t
        let l1 = List.map (fun x -> h::x) l
        (x::h::t)::l1

let rec permute = function
    | [] -> [[]]
    | [x] -> [[x]]
    | h::t -> 
        permute t
        |> List.map (fun l -> ins h l)
        |> List.concat

[1..5] |> permute

let counter n =
    let x = ref n
    fun () -> x := !x+1; 0

seq {1..10} |> Seq.fold ( * ) 1
let fact (n:int) = (..) 1 >> Seq.reduce ( * )
fact 5

let fact' = Seq.initInfinite (fun x-> x+1)
            |> Seq.scan ( * ) 1

// кококорекурсия
let rec nat =
    seq {
        yield 1
        yield! Seq.map ((+) 1) nat
    }

nat