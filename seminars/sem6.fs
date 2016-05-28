let rec private cc' amount coins = 
    match (amount, coins) with
    | (0, _) -> 1
    | (_, []) -> 0
    | (x, h::t) -> 
            if amount >= h
            then cc' (x - h) coins  + (cc' x t)
            else cc' amount (List.tail coins)

let cc (amount:int) (coins:int list) = 
    cc' amount (List.sort coins)

cc 5 [1; 2; 3; 5]

type 'a Tree =
    EmptyTree
    | Node of 'a * 'a Tree * 'a Tree

let rec tree2list = function
    | EmptyTree -> []
    | Node(n, l, r) -> tree2list l @ (n :: tree2list r)

let tree = Node(1, Node(2, EmptyTree, EmptyTree), Node(-9, Node(0, EmptyTree, EmptyTree), EmptyTree))
tree2list tree

type 'a TreeI =
    EmptyTreeI
    | NodeI of 'a * int * 'a TreeI * 'a TreeI

let rec tree2listI = function
    | EmptyTreeI -> []
    | NodeI(n, i, l ,r) -> tree2listI l @ List.replicate i n @ tree2listI r

let tree2 = NodeI(1, 3, NodeI(2, 0, EmptyTreeI, EmptyTreeI), NodeI(-9, 4, NodeI(0, 5, EmptyTreeI, EmptyTreeI), EmptyTreeI))
tree2listI tree2

