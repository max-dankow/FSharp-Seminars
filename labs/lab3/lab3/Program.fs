// подключаем FSharp.Data
#r "../packages/FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized
open System.Threading

// почтовый адрес
let email = ""

let attr_predicat (attr_name : string) (attr_value : string) (x : HtmlNode) =
    let attribute = x.TryGetAttribute(attr_name)
    match attribute with
    | None -> false
    | Some(x) -> if x.Value() = attr_value 
                 then true
                 else false

let get_next_page (html_source : HtmlDocument) : HtmlDocument Option =
    Thread.Sleep(10000);
    let is_disabled = html_source.Descendants ["span"] |> Seq.exists (fun x -> x.HasClass "next_page disabled")
    if is_disabled
    then None
    else
        let next_page = html_source.Descendants ["a"] 
                        |> Seq.find (fun x -> x.HasClass "next_page") 
                        |> HtmlNode.attribute("href") 
                        |> HtmlAttribute.value
        Some(HtmlDocument.Load("https://github.com" + next_page))

let get_repos_from_page (html_source : HtmlDocument) =
    html_source.Descendants["div"]
                |> Seq.filter (attr_predicat "class" "repo-list-stats") 
                |> Seq.map (fun x -> let name = (HtmlNode.innerText(x).Split([|' '|]) |> Array.filter (fun x -> x.Length>0)).[0] 
                                     name = "F#")
                |> Seq.toList
                //printfn "%s" (HtmlNode.innerText(x).Split([|' '|]).[0]) ;

let first_page = HtmlDocument.Load("https://github.com/search?o=desc&q=language%3AF%23+language%3AHaskell&ref=searchresults&s=stars&type=Repositories&utf8=")

let rec stupid page rest acc =
    if rest <= 0
    then acc
    else
    match page with
    | None -> acc
    | Some(x) -> let add = get_repos_from_page x
                 stupid (get_next_page x) (rest - List.length add) (acc @ add)

let top_number = 11
let top_n = stupid (Some(first_page)) top_number [] |> List.take top_number |> List.map (fun x -> if x then 1 else 0)
let answer = (float (List.sum top_n)) / (float (List.length top_n))

let lab3 () =
  let bases = HtmlDocument.Load("http://mipt.ru/diht/bases/")
  bases.Descendants ["ul"] 
    |> Seq.filter (fun x -> x.HasClass("right-menu")) 
    |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
    // для получения ссылок вместо InnerText нужно использовать методы TryGetAttribute, Attibute или AttributeValue
    // см. исходный код https://github.com/fsharp/FSharp.Data/blob/master/src/Html/HtmlOperations.fs
    |> Seq.map(fun x -> x.InnerText()) 
    |> Seq.toList

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab3().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.143.158:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString 