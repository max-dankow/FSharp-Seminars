let new_gen n = 
    let x = ref n in
    fun () -> x := !x + 1; !x

let rec take gen = function
    | 0 -> []
    | n -> gen () :: take gen (n-1)

let tri = new_gen 3
take tri 100

// Sequences
// Подсчет числа пи методом монте-карло
let rnd = System.Random()
let xs = Seq.initInfinite (fun _ -> rnd.NextDouble())
let ys = Seq.initInfinite (fun _ -> rnd.NextDouble())
let points = Seq.zip xs ys

let pi p = 
    let s = Seq.take p points
            |> Seq.filter (fun (x, y) -> x*x + y*y <= 1.)
            |> Seq.length in
    4. * float s / float p

pi 300000

//****************************************//
let fac = (..) 1 >> Seq.reduce ( * ) // явный оператор (1..n)
fac 5

// корекурсия
let rec ones = seq {
    yield 1;
    yield! ones;
}
ones |> Seq.take 10 |> Seq.toList

let rec private fac' = seq {
    yield (1, 1);
    yield! Seq.map (fun (value, x) -> (value * (x + 1), x + 1)) fac'
}

let coFac = Seq.map fst fac' 
coFac |> Seq.take 10 |> Seq.toList

// Вычисления с продолжением
1 |> (+) 1 |> ( * ) 2 |> printf "%d\n" 
let plus1 x f = f (x + 1)
let mul2 x f = f (x * 1)
plus1 1 (fun x -> mul2 x (printf "%d\n"))
plus1 1 <| mul2 <| printfn "%d"
