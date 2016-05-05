open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = ""

let explode (s:string) =
  [for c in s -> c]
  
// ************************** Разбиение на лексемы ********************************** //

type Token =
  | OpenBrace | CloseBrace
  | OpenBracket | CloseBracket
  | Colon | Comma
  | String of string
  | Number of int
  | Boolean of bool
  | Null

let tokenize source =
  let rec parseString acc = function
    | '\\' :: '"' :: t -> parseString (acc + "\"") t
    | '\\' :: 'n' :: t -> parseString (acc + "\n") t
    | '"' :: t -> acc, t  //"
    | c :: t -> parseString (acc + c.ToString()) t
    | _ -> failwith "Malformed string."
 
  let rec token acc = function
    | (x :: _) as t when List.exists ((=)x) [')'; ':'; ','; ']'] -> acc, t
    | w :: t when Char.IsWhiteSpace(w) -> acc, t
    | [] -> acc, [] // end of list terminates
    | c :: t -> token (acc + (c.ToString())) t

  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t
    | '{' :: t -> tokenize' (OpenBrace :: acc) t
    | '}' :: t -> tokenize' (CloseBrace :: acc) t
    | '[' :: t -> tokenize' (OpenBracket :: acc) t
    | ']' :: t -> tokenize' (CloseBracket :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '"' :: t -> // start of string"
      let s, t' = parseString "" t
      tokenize' (String s :: acc) t'
    | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
    | d :: t -> // остались числа
      let n, t' = token (d.ToString()) t
      tokenize' (Number (try Convert.ToInt32 n with e -> 0)  :: acc) t'
    | [] -> List.rev acc
    | _ -> failwith "Tokinzation error"
  tokenize' [] source

// ************************** Построение дерева ************************************* //

type JSON =
  | Object of (string * JSON) list
  | Array of JSON list
  | Number of int
  | String of string
  | Boolean of bool
  | Null

let rec parse_tokens tokens =
  let rec parse' tokens =
    let rec parseObject list = function
      | CloseBrace :: t -> (Object (List.rev list)), t
      | Comma :: t -> parseObject list t
      | Token.String s :: Colon :: t ->
        let value, rest = parse' t
        parseObject ((s, value) :: list) rest
      | _ -> failwith "Incorrect object"

    let rec parseArray list = function
      | CloseBracket :: t -> (Array (List.rev list)), t
      | Comma :: t -> parseArray list t
      | value -> 
        let a, t = parse' value
        parseArray (a :: list) t

    match tokens with
      | OpenBrace :: t -> parseObject [] t
      | OpenBracket :: t -> parseArray [] t
      | Token.Null :: t -> JSON.Null, t
      | Token.String s :: t -> JSON.String s, t
      | Token.Number s :: t -> JSON.Number s, t
      | Token.Boolean s :: t -> JSON.Boolean s, t
      | _ -> failwith "Incorrect identification"
  match parse' tokens with
    | res, [] -> res
    | _ -> failwith "Wrong JSON structure"

let parse_str = explode >> tokenize >> parse_tokens

// ************************** Вариант 12 ******************************************** //
let inRange a b (x: int) : bool = 
    (a <= x) && (x <= b)

let rec task12 a b = function
    | JSON.Object list -> list |> List.exists (snd >> task12 a b >> not) |> not
    | JSON.Array values -> values |> List.exists (task12 a b >> not) |> not
    | JSON.Number value -> inRange a b value
    | _ -> true

// ************************** Сериализация ****************************************** //
let offset (n: int) : string =
    if ( n = 0 )
    then String.Empty
    else [for i in [1..n] -> "    "] |> List.reduce (+)

let rec private stringify' (level: int) (json: JSON) : string = 
    let current_offset = offset level
    let next_offset = offset (level + 1)
    match json with
    | JSON.Object list -> 
        "{\n"        
        + (list |> List.map (fun (name, value) -> 
            next_offset + "\"" + name + "\": " + (stringify' (level + 1) value)) 
          |> List.reduce (fun s1 s2 -> s1 + ",\n" + s2))
        + "\n" + current_offset + "}"
    | JSON.Array values ->
        "[\n"        
        + (values |> List.map (fun (value) -> next_offset + (stringify' (level + 1) value)) 
           |> List.reduce (fun s1 s2 -> s1 + ",\n" + s2))
        + "\n" + current_offset + "]"
    | JSON.Number number -> number.ToString()
    | JSON.String s -> "\"" + s + "\""
    | JSON.Boolean b -> b.ToString()
    | JSON.Null -> "null"
let stringify = stringify' 0

// ************************** Ручное тестирование *********************************** //
let test_JSON_conversion s =
    try
        let applied = s |> parse_str |> stringify
        if ( applied |> parse_str |> stringify = applied) 
        then printfn "%s" applied
        else failwith "ERROR"
    with
        | _ -> printfn "Invalid argiment. Program is OK."

open System.IO
let path_to_tests = "programming/fp/labs/tests/" in
let manual_tests = ["test1"; "test2"; "test3"; "test4"] |> List.map ((+) path_to_tests) |> List.map (File.ReadAllText)

List.iter (test_JSON_conversion) manual_tests

// ************************** Генерация случайного дерева *************************** //
let random_char = 
    let rnd = new Random()
    let chars = List.concat [['a'..'z'];['A'..'Z'];['0'..'9']]
    let char_number = chars.Length
    Seq.initInfinite (fun _ -> chars.[rnd.Next(char_number)])

let random_str len = Seq.take len random_char |> Seq.map (string) |> Seq.reduce (+)

let rnd = new Random()
let rec generate rest : JSON = 
    let next_depth = rest - 1;
    let rand_max = if next_depth = 0 then 4 else 9
    let rand_min = if rest >= 5 then 4 else 0
    match rnd.Next(rand_min, rand_max) with
        | 0 -> JSON.Number (rnd.Next())  // number
        | 1 -> JSON.String (random_str (rnd.Next 20 + 1))  // string
        | 2 -> JSON.Boolean (rnd.Next 2 = 1)
        | 3 -> JSON.Null
        | 4 | 5 | 6 -> let number = rnd.Next(5) in
                       JSON.Object [for i in 0..number -> (random_str (rnd.Next 20 + 1), generate next_depth)] 
        | 7 | 8 -> let number = rnd.Next(5) in
                   JSON.Array [for i in 0..number -> generate next_depth]
generate 5 |> stringify |> printfn "%s"

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.143.158:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
