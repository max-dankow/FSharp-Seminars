// Генерация списков.
let l = [1..10]
let l2 = [1..2..10]
let l3 = [for i in l do yield i * i]

let prodl = [for a in [1..10] do
                for b in [1..10] do
                    yield (a,b)]

// -> <=> do yield
let list9 = [for a in 1..3..5 do yield! [a..a+2]]

// Последовательности. Высчитываются только когда нужно.
let seq1= seq {1..78}
Seq.iter (printf "%d ") seq1
let bigSeq = {1I .. 10000000000000000000000000I}
//let oopsList = [1I .. 10000000000000000000000000I]

// Демонстрация ленивых вычислений.
let seq4 = seq {
    for a in 1 .. 10 do
            printfn "created %A\n" a
            yield a
}

seq4 |> Seq.take 3
seq4 |> Seq.take 4 // Пересоздает

let devByZero = [for a in 5 .. -1 .. 0 -> 10 / a] // Деление на 0
let seq5 = seq {for a in 5 .. -1 .. 0 -> 10 / a}

let rec even =
    seq {
        yield 1
        yield! Seq.map (( * ) 2) even
    }

let seq6 = 
    let rec seq6' x = seq {yield x; yield! seq6' (x+2)}
    seq6' 0

Seq.item 5000 seq6

let sq = Seq.initInfinite (fun x -> x*x)

Seq.item 900 sq

let list10 = Seq.unfold (fun oldState -> 
                        if oldState < 100
                        then Some(oldState, oldState + 2)
                        else None) 2
Seq.take 49 list10

// unfold: old -> Optional(current, next). Заканчивается когда None
let fibSeq = Seq.unfold (fun (lastlast, last) -> Some(lastlast + last, (last, lastlast + last))) (0, 1)

let maxRowSum = 
    List.mapi (fun i l -> (i, List.sum l)) 
    >> List.maxBy (fun (i, sum) -> sum) 
    >> fst

let curry f (a, b) = f a b  // Каррирует функцию
let zipWith f a b = List.zip a b |> List.map (curry f)
let maxColSum = 
    List.reduce (fun acc l -> zipWith (+) acc l)
    >> List.mapi (fun i sum -> (i, sum))
    >> List.maxBy (fun (i, sum) -> sum)
    >> fst

let matrix = [[1000;2;999]; [9;8;2]; [11; 99; 1]; [-10000; -100; -99999990]]
maxColSum matrix
maxRowSum matrix