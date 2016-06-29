// Вычисления с продлжением
type 'a Tree =
    EmptyTree
    | Node of 'a * 'a Tree * 'a Tree

let rec tree2list = function
    | EmptyTree -> []
    | Node(n, l, r) -> tree2list l @ (n :: tree2list r)

let tree = Node(1, Node(2, EmptyTree, EmptyTree), Node(-9, Node(0, EmptyTree, EmptyTree), EmptyTree))
tree2list tree

let foldTree f init tree = 
    let rec fold' tree cont = // cont - накопленная композиция функций
        match tree with
        | EmptyTree -> cont init
        | Node (x, left, right) -> fold' left (fun leftValue ->
            fold' right (fun rightValue ->
                cont (f x leftValue rightValue)
            )
          )
    fold' tree (fun x -> x)
let sumTree = foldTree (fun x left right -> x + left + right) 0

// дз : свернуть дерево в другое дерево

// Set
type Set_ = int -> bool
let (a:Set_) = fun x -> true
let contains (s:Set_) (a:int) = s a

// Множество из одного элемента
let singletonSet (b : int) : Set_ = fun a -> a = b
let union (a:Set_) (b:Set_) = fun x -> a x || b x
let intersect (a:Set_) (b:Set_) = fun x -> a x && b x
let filter (a:Set_) f = fun x -> a x && f x

let forAll (a:Set_) (f:int -> bool) =
    List.filter (filter a (not << f)) [-1000..1000] |> List.isEmpty

let exists (a:Set_) f = not (forAll a (not << f))

let mySet = union (singletonSet 10) (union (singletonSet 11) (singletonSet 12))

forAll mySet ( fun x -> x > 10)

//contains (singletonSet 10) 10
//let ten11 = union (singletonSet 10) (singletonSet 11)
// DZ - list to set_
let listToSet (l: int list) : Set_ =
    fun x -> List.contains x l

let printSet (s:Set_) : unit =
    [-1000..1000] |> List.filter s |> List.iter (printfn "%d")

listToSet [-1..100]
|> printSet

// Парсинг JSON
