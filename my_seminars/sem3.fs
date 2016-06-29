let rec zip a b = 
    match (a,b) with
    | ([], _) | (_, []) -> []
    | (x::xs, y::ys) -> (x, y)::(zip xs ys)

let rec append l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> h :: (append t l2)

append [1; 2] [3; 5; 0]

let rec private fac' acc = function
    | x when x<=1 -> acc
    | x -> fac' (acc * x) (x-1)

let fac = fac' 1

// (TR) Инвертирует список.
let rec private reverse' acc = function
    | [] -> acc
    | h::t -> reverse' (h::acc) t
let reverse = reverse' []

// Возвращает список букв строки.
let explode s = 
    [for c in s -> c]

// Собирает строку из списка символов.
let rec implode2 = function
    | [] -> ""
    | h::t -> h.ToString() + (implode2 t)

// (TR) Собирает строку из списка символов.
let rec private implode' acc = function
    | [] -> acc
    | h::t -> implode' (acc + h.ToString()) t
let implode = implode' ""

(explode >> implode) "abacababa"

// Удаляет все вхождения буквы c из строки.
let deleteLetter c =
    let rec delete' c = function
        | [] -> []
        | h::t when h<>c-> h::(delete' c t)
        | h::t -> delete' c t
    explode >> (delete' c) >> implode