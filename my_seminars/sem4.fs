let explode s = 
    [for c in s -> c]


let parse (s:string) = 
    let rec  parse' (acc:char list) = function
        | [] -> true
        | x::xs when x = '(' -> 
        parse' (x::acc) xs
        | x::xs when x = ')' ->
            if List.length acc = 0 //  Если нечего закрывать
            then false
            else parse' (List.tail acc) xs
        | _::xs ->
            parse' acc xs
    explode s |> parse' []

let rec reverseAll list=
    List.rev (List.map (List.rev) list)

let doItAgain f x = f (f x)

List.reduce (+) [1..10]

let scanBack f list init =
    let rec scanBack' f = function
        | [] -> [init]
        | x::xs -> 
            let r = scanBack' f xs in
            (f x (List.head r))::r
    scanBack' f list

let scan f init list = 
    let rec scan' acc f = function
        | [] -> acc
        | x::xs -> 
            let cur = f (List.head acc) x in
            scan' (cur::acc) f xs
    scan' [init] f list |> List.rev

scanBack (-) [1..5] 0
List.scanBack (-) [1..5] 0

scan (-) 0 [1..5]
List.scan (-) 0 [1..5]