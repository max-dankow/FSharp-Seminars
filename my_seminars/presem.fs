let lambda = fun (x) -> x*x + 1

let plus (a, b) = a + b

// Каррирование
let cplus a b = a + b

let inc = (+) 1

type SolveResult = 
    |None
    |Linear of float
    |Quadratic of float * float

let solve a b c = 
    let D = b * b - 4. * a * c in
    if a = 0.
    then
        if b = 0.
        then None
        else Linear(-c/b)
    else 
        if D < 0.
        then None
        else Quadratic(((-b + sqrt(D)) / (2. * a), (-b - sqrt(D)) / (2. * a)))

let textResult = function
    |None -> printfn "No solution"
    |Linear(x) -> printfn "Linear, root is %f" x
    |Quadratic(x1, x2) when x1 = x2 -> printfn "Quadratic, root is %f" x1
    |Quadratic(x1, x2) -> printfn "Quadratic, roots are %f, %f" x1 x2 

let solveLinear = solve 0.

textResult (solveLinear -2. 1.)

let rec printRange a b = 
    printf "%d " a
    if a < b
    then printRange (inc a) b
        
let rec forLoop f a b = 
    f a
    if a < b
    then forLoop f (inc a) b

printRange 1 10
printf "\n"

let printX = fun (x) -> 
    printf "%d " x

forLoop printX 1 10
printf "\n"

for i = 1 to 10 do printf "%d " i
printf "\n"

let rec repeat n f = 
    if n = 0
    then fun x -> x
    else f >> (repeat (n - 1) f)

repeat 16 ((+) 1) 99

let sum x y = 
    repeat x ((+) 1) y

sum 10 100
