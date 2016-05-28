#r "../packages/FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
open FSharp.Data

open System
open System.IO
open System.Net

let explode (s:string) =
  [for c in s -> c]

// (TR) Собирает строку из списка символов.
let rec private implode' acc (s : char list) = 
    match s with
    | [] -> acc
    | h::t -> implode' (acc + h.ToString()) t
let implode = implode' ""



// Счтитывание файла из интернет
let wc = new WebClient()
wc.DownloadFile("http://www.yandex.ru", @"./text_from_net.txt")

// Считываем файл.
let file2letters path =
    File.ReadAllText path |> explode |> List.filter (fun c -> Char.IsLetter c) |> List.map (Char.ToLower)

// Отсортированный частотный словарь.
let build_dict (letters : string) =
    letters |> explode |> List.filter(Char.IsLetter) 
    |> List.groupBy (fun x -> x)  |> List.map (fun (v, l) -> (v, l.Length)) |> List.sortBy (snd >> ( * ) -1)

build_dict (File.ReadAllText "./text_from_net.txt")

// Кодирование файла
type Table = (char * char) list

let match_letter (table : Table) (c : char) : char =
    let has = table |> List.map fst |> List.contains c
    if has
    then table |> List.find (fun (i, o) -> i = c) |> snd
    else c
// Кодируюащая таблица - сортирующая
let russian = build_dict (File.ReadAllText "./text.txt")
let header = russian |> List.map fst
let alphabet = header |> List.sort
let sortT : Table = List.zip alphabet header

// Таблица декодирования - обратная к таблице кодирования, предполагаем биекцию
let invert (table : Table) : Table =
    table |> List.map (fun (i, o) -> (o, i))

let encode (input : string) (table : (char * char) list) =
    input |> explode |> List.map (match_letter table) |> implode

let encode_file (input : string) (output : string) (table : Table) = 
    let result = encode (File.ReadAllText input) table
    File.WriteAllText (output, result)
    result
encode_file "./text2.txt" "./rr.txt" sortT

let shuffle l =
    let rand = new System.Random()

    let swap (a: char []) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffleA a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a
    let arr = List.toArray l
    shuffleA arr;
    Array.toList arr


let generate() = List.zip alphabet (shuffle alphabet)

let encode_file_random (input : string) (output : string) = 
    let random_table = generate()
    encode_file input output random_table

let auto (s : string) : (string * Table) =
    let this = build_dict s
    let v = russian |> List.take (this.Length) |> List.map fst
    let r = List.zip (this |> List.map fst) v
    (encode s r, r)

let decode_file_auto (input : string) (output : string) = 
    let outFile = new StreamWriter(output)
    let result = File.ReadAllText input |> auto
    outFile.Write result

let demo() =
    let data = File.ReadAllText "./test3.txt"
    let table = generate()
    let encoded = encode data table
    printfn "ENCODED:\n%s" encoded
    printfn "AUTO:\n%s" (auto encoded |> fst)
    printfn "CORRECT TABLE:\n%s" (encode encoded (invert table))

demo()

decode_file_auto "./text2.txt" "./decode.txt"
generate()