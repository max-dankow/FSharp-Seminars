// Монады.
open System
let read() = 
    printf ">"
    let s = Console.ReadLine()
    try 
        Some((int) s)
    with
     _ -> None

let bind a f = 
    if a = None
    then None
    else f a.Value

bind (read()) (fun x -> bind (read()) (fun y -> Some(x + y)))

type Nondet<'t> = 't list

let (>>=) a f = List.collect f a

[15; 16] >>= (fun x -> [x * 2; x * 3]) >>= (fun x -> [x + 1])

type NondetBuilder() =
    member b.Return(x) = printfn "return %A" x; [x]
    member b.Bind(mA, f) = printfn "bind %A with %A" mA f;List.collect f mA
let nondet = new NondetBuilder()

nondet {
    let! vasya = [15;16]
    let! petya = [2 * vasya; 3 * vasya]
    let lena = petya + 1
    return lena
}

let myFilter f l= List.map (fun x -> if f x then [x] else []) l |> List.reduce (@)

[1..100] |> myFilter (fun x -> x%10 = 0)

let mf f s =
    if f (String.length s)
    then s
    else s + s

["123";"lolka";"da"] |> List.map (mf (fun x -> x%2 = 0))

let list = [for a in 1..100 do if a%3=0 && a%5=1 then yield a]

let flip f x y = f y x


let S x y z = x z (y z)
let K x y = x
let mod_ n = flip (%) n
let cond = S (mod_ 5 >> (=) 1 >> (&&)) (mod_ 3 >> (=) 0)

let list_ = [1..100] |> List.filter (cond)
let s = seq{yield 1}

let myFilter2 f l = List.foldBack (fun x acc -> if f x then x::acc else acc) l []
[1..100] |> myFilter2 (fun x -> x%10 = 0)
