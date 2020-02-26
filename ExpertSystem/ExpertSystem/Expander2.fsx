#I @"e:\Project\FsharpMyExtension\FsharpMyExtension\FsharpMyExtension\bin\Debug\net461\"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
#r @"Fuchu.dll"
#r @"HtmlAgilityPack.dll"
#r @"Newtonsoft.Json.dll"
#r @"Newtonsoft.Json.Bson.dll"
#r @"FsharpMyExtension.dll"

#if INTERACTIVE
#load "expander.fs"
#endif
open FsharpMyExtension
open FsharpMyExtension.Tree

type NameProd = string
type CountProd = int
type Prod = { Name:string; ProdCount:int; Ingr:Map<NameProd,CountProd> }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Prod =
    let getName { Name = x } = x
    let getProdCount { ProdCount = x } = x
    let getIngr { Ingr = x } = x

/// а что оно делает?..
let expand recipes = 
    let rec expa last ((name, count:CountProd) as curr) = 
        let x = Map.find name recipes
        float x.ProdCount / float count |> ceil |> int |>
        s (fun coeff ->
            cond (fun ingrs ->
                    List.forall (not << flip Map.containsKey ingrs) last )
                 (Map.toList
                  >> List.map (expa (name::last))
                  >> uncurry Tree.Node (name, x.ProdCount * coeff))
                 (fun _ -> uncurry Tree.Node curr []))
          (fun coeff -> Map.map (fun _ -> (*) coeff) x.Ingr)
    expa []

assert
    [ { Name = "a"; ProdCount = 1; Ingr = Map["b", 1]; }
      { Name = "b"; ProdCount = 1; Ingr = Map["c", 2]; }
      { Name = "c"; ProdCount = 1; Ingr = Map[]; } ]
    |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq |> flip expand ("a", 1)
    true

type 'a Prod2 = { P:Prod; Price:'a }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Prod2 =
    let getProd { P = p } = p
    let getIngr { P = p } = p.Ingr
    let getPrice { Price = x } = x
    let getName { P = x } = x.Name
    let getProdCount { P = p } = p.ProdCount

type RecipesType = (string * (int * (string * int) list)) list
let recipes (lst: RecipesType) =
    lst
    |> List.map (fun (x, (count, xs)) -> List.partition (fst >> (=) "gold") xs |> (function [], _ -> None, (x, (count, xs)) | [_,n], xs -> Some n, (x, (count, xs)) | x -> failwithf "many golds:\n%A" x))
    |> List.fold (fun st (g, (name,(count, ingr))) -> Map.add name { P = { Name = name; ProdCount = count; Ingr = Map.ofList ingr }; Price = g; } st ) Map.empty
//
//let remNotPrice m = Map.choose (fun _ x -> Prod2.getPrice x |> Option.bind (fun p -> if Prod2.getIngr x |> Map.forall (fun name _ -> Map.find name m |> (Prod2.getPrice >> Option.isSome)) then Some { P = x.P; Price = p; } else None ) ) m
//
//let recip lst =
//    recipes lst |> //fun recipes ->
//    s (fun recipes recipesP -> recipes |> Map.map (fun name x -> expand recipesP (name, Prod2.getProdCount x) |> fun t -> Tree.leafs t |> fun xs -> { x with P = { x.P with Ingr = Map.ofList xs; ProdCount = Tree.get t |> snd }}) |> remNotPrice)
//      (Map.map <| konst id Prod2.getProd)
//    (*recipes |> Map.map (konst id Prod2.getProd) |> fun recipesP ->
//    recipes |> Map.map (fun name x -> expand recipesP (name, Prod2.getProdCount x) |> fun t -> Tree.leafs t |> fun xs -> { x with P = { x.P with Ingr = Map.ofList xs; ProdCount = Tree.get t |> snd }}) |> remNotPrice *)
//    //recipes lst |> Map.map (fun name x -> failwith "" )


let makeBased m = m |> s (fun recipes recipesP -> recipes |> Map.map (fun name x -> expand recipesP (name, Prod2.getProdCount x) |> fun t -> Tree.leafs t |> fun xs -> { x with P = { x.P with Ingr = Map.reducef fst (snd >> (+)) 0 Map.empty xs; ProdCount = Tree.getValue t |> snd }})) (Map.map <| konst id Prod2.getProd)


let recipWithPrices = 

    let remNotPrice m = Map.choose (fun _ x -> Prod2.getPrice x |> Option.bind (fun p -> if Prod2.getIngr x |> Map.forall (fun name _ -> Map.find name m |> (Prod2.getPrice >> Option.isSome)) then Some { P = x.P; Price = p; } else None ) ) m
    fun m -> (makeBased >> remNotPrice) m

let awer given recip =
    recip |> Map.fold (fun st _ x -> if Map.containsKey (Prod2.getName x) given then st else Prod2.getIngr x |> Map.fold (fun st name count -> Map.tryFind name st |> Option.map (mapFst (fun xs -> (Prod2.getName x,count)::xs) >> fun x -> Map.add name x st) |> Option.defaultWith (fun () -> st) ) st ) given

let p recip give given = recip |> Map.fold (fun st _ x -> if Map.containsKey (Prod2.getName x) given then st else if Prod2.getIngr x |> Map.forall (konst <| fun name -> give |> List.exists (fst >> (=) name)) then on Prod2.getName Prod2.getPrice x :: st else st ) []

let convertToMaple give given recip =
    let m,prof = p recip give given |> List.mapi (fun i (name, x) -> sprintf "x%d" i |> fun y -> (name, y), (y, x)) |> List.unzip |> mapFst Map.ofList
    let xs = awer given recip |> Map.toList |> List.map (snd >> mapFst (List.choose (fun (x,y) -> Map.tryFind x m |> Option.map (flip comma y)))) |> List.filter (fst >> List.isEmpty >> not)
    let fn = List.map (curry (flip <| sprintf "%d*%s")) >> String.concat " + "
    let prof' = prof |> fn
    [xs |> List.map (mapFst fn >> curry (sprintf "%s <= %d")) |> String.concat ", "; prof'] |> fun xs -> System.IO.File.WriteAllLines(@"e:\forMaple.txt", xs)
    m

let readFromMaple (recipes:RecipesType) m =
    let asd =
        let s = System.IO.File.ReadAllText @"e:\fromMaple.txt"
        s |> fun x -> System.Text.RegularExpressions.Regex.Matches(x, "(x\d+) = (\d+)") |>  Seq.cast<System.Text.RegularExpressions.Match> |> Seq.map (fun x -> if x.Success then x.Groups |> fun x -> x.[1].Value, int x.[2].Value  else failwithf "%A" x) |> List.ofSeq
    let unterpm = m |> Map.toList |> List.map (curry <| flip comma) |> Map.ofList // |> ( List.map << flip Map.find)
    asd |> List.map (mapFst <| flip Map.find unterpm) |> List.filter (snd >> ((<>) 0))

let give recipes =
    System.IO.File.ReadAllLines(@"e:\currstate.txt", System.Text.Encoding.GetEncoding "windows-1251") |> List.ofArray |> List.map (FsharpMyExtension.Show.split '\t' >> function [|name; count|] -> name, (System.Text.RegularExpressions.Regex.Matches(count, "\d+") |> cond (function null -> false | _ -> true) (Seq.cast<System.Text.RegularExpressions.Match> >> Seq.sumBy(fun x -> int x.Value)) (fun _ -> failwithf "%A" name)) | x -> failwithf "%A" x)
    //|> List.head |> fst |> flip Map.tryFind recipes
    |> fun xs -> xs |> List.map (fst >> s (fun x -> cond Option.isNone (fun _ -> printfn "%A" x; recipes |> Map.exists (fun _ v -> Prod2.getIngr v |> Map.exists (konst ((=) x))) |> cond id (konst id ()) (fun _ -> failwithf "not found %A" x)) (konst id ())) (flip Map.tryFind recipes)) |> ignore; xs

let given give = give |> List.map (mapSnd (fun c -> [], c)) |> Map.ofList

let res () =
    let lst = System.IO.File.ReadAllText @"e:\Project\ExpertSystem\ExpertSystem\bin\Debug\bd.dat" |> fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<RecipesType> x
    let recipes' = recipes lst
    //(Map.map <| konst id Prod2.getProd) recipes' |> flip expand ("картофельная решетка", 1)
    let recip = recipWithPrices recipes'

    let m =
        give recipes' |> fun give ->
        given give |> fun given ->
        convertToMaple give given recip
    //Map.find "картофельная решетка" m
    let res = readFromMaple lst m
    let profit = List.map (s (snd >> (*) >> (>>) Prod2.getPrice) <| (fst >> flip Map.find recip)) >> List.sum
    profit res
    let r = Expander.expand2Start (Map.ofList lst |> Map.add "крафт" (1, res)) Map.empty ("крафт", 1)
    true

    
let asdf lst =
    //let sd m = Map.fold (fun st k -> Prod2.getIngr >> Map.toSeq >> Map.fn fst (fun _ v -> k::v) [] st) Map.empty m

    recipes lst |> Map.fold (fun st k -> Prod2.getIngr >> Map.toSeq >> Map.reducef fst (fun _ v -> k::v) [] st) Map.empty
//let given = give |> List.map (mapSnd (fun c -> [], c)) |> Map.ofList

