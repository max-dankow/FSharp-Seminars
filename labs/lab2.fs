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

let s = """
{
   "firstName": "Иван",
   "lastName": "Иванов",
   "address": {
       "streetAddress": "Московское ш., 101, кв.101",
       "city": "Ленинград",
       "postalCode": 101101
   },
   "phoneNumbers": [
       "812 123-1234",
       "916 123-4567"
   ]
}
"""

let parse_str = explode >> tokenize >> parse_tokens
let json = parse_str s

// ************************** Вариант 12 ******************************************** //
let inRange a b (x: int) : bool = 
    (a <= x) && (x <= b)

let rec task12 a b = function
    | JSON.Object list -> list |> List.exists (snd >> task12 a b >> not) |> not
    | JSON.Array values -> values |> List.exists (task12 a b >> not) |> not
    | JSON.Number value -> inRange a b value
    | _ -> true

let res = s |> parse_str |> task12 0 10

// ************************** Сериализация ****************************************** //
let offset (n: int) : string =
    if ( n = 0 )
    then String.Empty
    else [for i in [1..n] -> "    "] |> List.reduce (+)

let rec stringify (level: int) (json: JSON) : string = 
    let current_offset = offset level
    let next_offset = offset (level + 1)
    match json with
    | JSON.Object list -> 
        "{\n"        
            + (list |> List.map (fun (name, value) -> next_offset + "\"" + name + "\": " + (stringify (level + 1) value)) |> List.reduce (fun s1 s2 -> s1 + ",\n" + s2))
        + current_offset + "\n}"
    | JSON.Number number -> number.ToString()
    | String s -> "\"" + s + "\""
    | Boolean b -> b.ToString()
    | Null -> "null"
    | _ -> "fuckoff"

printfn "%s" (s |> parse_str |> stringify 0)

let generate = 
  let rnd = new Random()
  match rnd.Next(42) with
    | 0 -> Object []
    | _ -> Object [("random", Object [])]

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.143.158:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString


  (*
  let fst3 (a, b, c) = a
let snd3 (a, b, c) = b
let thrd3 (a, b, c) = c

let wlReturn (x: 'a) : ('a * int * string) = (x, 0, String.Empty)
let wlReturnFrom (x: 'a * int * string) : 'a * int * string = x
let wlBind (mx: ('a * int * string)) (f: 'a -> 'b * int * string) : 'b * int * string = 
    let res = f (fst3 mx)
    (fst3 res, 0, thrd3 mx + thrd3 res)

type WriteWithLevelMonad() =
    member x.Bind(p, f) = wlBind p f
    member x.Return(y) = wlReturn y
    member x.ReturnFrom(y) = wlReturnFrom y

let writer = new WriteWithLevelMonad()



let squared x = (JSON.Null, 0, " NULL yopta ")
//let halved x = (x / 2, " was halved.")
 
let a = writer {
    let! a = wlReturn 4
    let! b = squared a
    return b
}
  *)