module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес
let email = "max.dankow@gmail.com"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть

let fTailor x : float = 3. ** x // функция, которую раскладываем f(x) = 3^x
let n, a, b = 20., 0., 1. // интервал [0; 1]

// Раскладывает функцию в ряд Тейлора, в лоб считая члены.
let private nthMember (x:float) k = 
    let fac = function 
    | 0 | 1 -> 1.
    | n -> [for k in [1..n] -> float k] |> List.reduce ( * )
    ((log 3. * x) ** float k) / float (fac k)

let rec private tailor_ acc x : Result =
    if abs(fst acc - fTailor x) < delta
    then acc
    else let k = snd acc in tailor_ (fst acc + nthMember x k,  k + 1) x

let tailor = tailor_ (0., 0)

// Раскладывает функцию в ряд Тейлора домножением на предыдущий член.
let private pow3_ x k (prev, sum) =
    let current = prev *  log 3. * x / (float k) in
    (current, sum + current)

let rec private tailorA_ x fnext acc = 
    let (prev, sum, k) = acc
    if abs(sum - fTailor x) < delta
    then acc
    else 
        let (cur, cursum) = fnext x k (prev, sum)
        tailorA_ x (fnext) (cur, cursum, k + 1)

let tailorA x : Result = let (prev, sum, k) = tailorA_ x (pow3_) (1., 1., 1) in (sum, k)

let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

printTailor()
// *** Вторая часть
let precision = 1e-10

// Метод итерации.
let iter f a b : Result =
    let rec iter' acc =
        let cur = f (fst acc)
        if abs(cur - fst acc) < precision
        then (cur, snd acc + 1)
        else iter' (cur, snd acc + 1)
    iter' ((a + b) / 2., 0)

// Метод Ньютона.
let rec private newton' i f f' prev : Result = 
    let cur = prev - f prev / f' prev
    if abs(cur - prev) < precision
    then (cur, i)
    else newton' (i + 1) f f' cur
      
let newton f f' f'' a b = 
    if f a * f'' a > 0.
    then newton' 0 f f' b
    else newton' 0 f f' a

// Метод половинного деления.
let dichotomy =
    // для функций с аккумулятором удобно ставить его в начало
    let rec dichotomyA i (f:float->float) (a:float) (b:float) : Result = 
        let middle = (a + b) / 2. in
       
        if b < a
        then failwith "Wrong arguments"

        if b - a < precision
        then (middle, i)
        else 
            if (f a) * (f middle) < 0.
            then dichotomyA (i + 1) f a middle
            else 
                if (f b) * (f middle) < 0. 
                then dichotomyA (i + 1) f middle b
                else failwith "Wrong arguments"
    dichotomyA 0 // чтобы воспользоваться каррированием

//  Вариант 12.
let f12 = fun x -> log x - x + 1.8  // функция, решение которой ищем
let fi12 = fun x -> log x + 1.8  // f(x) = 0 <=> fi(x) = x
let f12' = fun x -> 1. / x - 1.  // ее первая производная.
let f12'' = fun x -> -1. / (x ** 2.)  // ее вторая производная
let l12, r12 = 2., 3.

let l = [(fun x->x, 10); (fun x->x, 0)]
let tasks = [((fun x -> log x - x + 1.8), (fun x -> log x + 1.8), (fun x -> 1. / x - 1.), (fun x -> -1. / (x ** 2.)), 2., 3.); 
             ((fun x -> log x - x + 1.8), (fun x -> log x + 1.8), (fun x -> 1. / x - 1.), (fun x -> -1. / (x ** 2.)), 2., 3.)]

let printSolve (f, fi, f', f'', l, r) =
    [iter fi l r; newton (f) (f') (f'') l r; dichotomy f l r] 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

let ln = fun task -> 
    printfn "*******************\n" 
    printSolve task
List.map (ln) tasks
(*let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://mipt.eu01.aws.af.cm/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString*)
