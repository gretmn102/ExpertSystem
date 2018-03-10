module Expander2

#if INTERACTIVE
#load "expander.fs"
#endif
open FsharpMyExtension.Tree
open FsharpMyExtension.FSharpExt
open FsharpMyExtension.List
open FsharpMyExtension.Map
open FsharpMyExtension.Option

type NameProd = string
type CountProd = int
type Prod = { Name:string; ProdCount:int; Ingr:Map<NameProd,CountProd> }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Prod =
    let getName { Name = x } = x
    let getProdCount { ProdCount = x } = x
    let getIngr { Ingr = x } = x

let expand reciples = 
    let rec expa last ((name, count:CountProd) as curr) = 
//        let p, ingrs = 
//            //(((>>) ((*) >> (<<) comma)) ^< flip s ^< (>>) ((*) >> konst id) ^< flip Map.map) ingrs p
//            Map.find name reciples |> fun { ProdCount = p; Ingr = ingrs } ->  // crafts.[name]
//            p |> (float >> flip (/) (float count) >> ceil >> int >> //fun coeff ->
//                s ((*) p >> comma) ((*) >> konst id >> flip Map.map ingrs)
//            )
        Map.find name reciples |> fun x -> 
        x.ProdCount |> float |> (/) (float count) |> ceil |> int |>
        s (fun coeff ->
            //(fun ingrs -> last |> List.forall ^| not ^< flip Map.containsKey ingrs)
            cond ((|>) last ^< List.forall ^< (<<) not ^< flip Map.containsKey )
                 (Map.toList >> List.map (expa <| name::last) >> uncurry Tree.Node (name, x.ProdCount * coeff))
                 (fun x -> uncurry Tree.Node curr [])) //(konst id <| uncurry Tree.Node curr []))
          //((*) >> konst id >> flip Map.map x.Ingr)
          (fun coeff -> Map.map (fun _ -> (*) coeff) x.Ingr)
        (*if last |> List.forall ^| not ^< flip Map.containsKey ingrs then
            Tree.Node((name, p), ingrs |> Map.toList |> List.map (expa (name::last)))
        else Tree.Node(curr, []) *)
    expa []

assert
    [ { Name = "a"; ProdCount = 1; Ingr = Map["b", 1]; }
      { Name = "b"; ProdCount = 1; Ingr = Map["c", 2]; }
      { Name = "c"; ProdCount = 1; Ingr = Map[]; } ] |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq |> flip expand ("a", 1)
    true

//assert
//    let (^<<) = (<<)
//    
//    
//    let s ingrs = //((|>) ingrs) ^<< Map.map ^<< konst id ^<< (*)
//        (|>) ingrs << Map.map ^<< konst id ^<< (*) // = flip (Map.map ^<< konst id ^<< (*)) ingrs
//        //(*) >> konst id >> flip Map.map ingrs
//
//    let m =
//        [
//            { Name = "a"; ProdCount = 2; Ingr = Map[ "b", 2; "c", 3 ]}
//            { Name = "b"; ProdCount = 3; Ingr = Map[ "c", 1; ]}
//            { Name = "c"; ProdCount = 1; Ingr = Map[] }
//        ] |> List.map (fun x -> x.Name, x) |> Map.ofList
//    expand m ("a", 1) |> Tree.visualize (sprintf "%A") |> printfn "%s"
//    true
//type T = { Name:string; ProdCount:int; Ingr:(NameProd*CountProd) list }
//let expand2 reciples = 
//    let rec expa last ((name, count) as curr) = 
//        let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty
//
//        let p, ingrs = 
//            let { ProdCount = p; Ingr = ingrs } = Map.find name reciples // crafts.[name]
//            let coeff = (float count/ float p) |> ceil |> int
//            coeff*p, ingrs |> List.map (function n, c -> n, coeff * c)
//        if valid last (List.map fst ingrs) then
//                Tree.Node((name, p), List.map (expa (name::last)) ingrs)
//        else Tree.Node(curr, [])
//    expa []
//Set.intersect (Set[1;2]) (Set[2;3;4])
//let m =
//    [
//        { Name = "a"; ProdCount = 2; Ingr = [ "b", 2; "c", 3 ]}
//        { Name = "b"; ProdCount = 3; Ingr = [ "c", 1; ]}
//        { Name = "c"; ProdCount = 1; Ingr = [] }
//    ] |> List.map (fun x -> x.Name, x) |> Map.ofList
//expand2 m ("a", 1) |> Tree.visualize (sprintf "%A") |> printfn "%s"
//
//

type 'a Prod2 = { P:Prod; Price:'a }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Prod2 =
    let getProd { P = p } = p
    let getIngr { P = p } = p.Ingr
    let getPrice { Price = x } = x
    let getName { P = x } = x.Name
    let getProdCount { P = p } = p.ProdCount

type ReciplesType = (string * (int * (string * int) list)) list
let reciples (lst: ReciplesType) =
    lst
    |> List.map (fun (x, (count, xs)) -> List.partition (fst >> (=) "gold") xs |> (function [], _ -> None, (x, (count, xs)) | [_,n], xs -> Some n, (x, (count, xs)) | x -> failwithf "many golds:\n%A" x))
    |> List.fold (fun st (g, (name,(count, ingr))) -> Map.add name { P = { Name = name; ProdCount = count; Ingr = Map.ofList ingr }; Price = g; } st ) Map.empty
//
//let remNotPrice m = Map.choose (fun _ x -> Prod2.getPrice x |> Option.bind (fun p -> if Prod2.getIngr x |> Map.forall (fun name _ -> Map.find name m |> (Prod2.getPrice >> Option.isSome)) then Some { P = x.P; Price = p; } else None ) ) m
//
//let recip lst =
//    reciples lst |> //fun reciples ->
//    s (fun reciples reciplesP -> reciples |> Map.map (fun name x -> expand reciplesP (name, Prod2.getProdCount x) |> fun t -> Tree.leafs t |> fun xs -> { x with P = { x.P with Ingr = Map.ofList xs; ProdCount = Tree.get t |> snd }}) |> remNotPrice)
//      (Map.map <| konst id Prod2.getProd)
//    (*reciples |> Map.map (konst id Prod2.getProd) |> fun reciplesP ->
//    reciples |> Map.map (fun name x -> expand reciplesP (name, Prod2.getProdCount x) |> fun t -> Tree.leafs t |> fun xs -> { x with P = { x.P with Ingr = Map.ofList xs; ProdCount = Tree.get t |> snd }}) |> remNotPrice *)
//    //reciples lst |> Map.map (fun name x -> failwith "" )


let makeBased m = m |> s (fun reciples reciplesP -> reciples |> Map.map (fun name x -> expand reciplesP (name, Prod2.getProdCount x) |> fun t -> Tree.leafs t |> fun xs -> { x with P = { x.P with Ingr = Map.reducef fst (snd >> (+)) 0 Map.empty xs; ProdCount = Tree.get t |> snd }})) (Map.map <| konst id Prod2.getProd)


let recipWithPrices = 

    let remNotPrice m = Map.choose (fun _ x -> Prod2.getPrice x |> Option.bind (fun p -> if Prod2.getIngr x |> Map.forall (fun name _ -> Map.find name m |> (Prod2.getPrice >> Option.isSome)) then Some { P = x.P; Price = p; } else None ) ) m
    fun m -> (makeBased >> remNotPrice) m

let awer given recip =
    recip |> Map.fold (fun st _ x -> if Map.containsKey (Prod2.getName x) given then st else Prod2.getIngr x |> Map.fold (fun st name count -> Map.tryFind name st |> Option.map (mapFst (fun xs -> (Prod2.getName x,count)::xs) >> fun x -> Map.add name x st) |> Option.getOrDef (fun () -> st) ) st ) given

let p recip give given = recip |> Map.fold (fun st _ x -> if Map.containsKey (Prod2.getName x) given then st else if Prod2.getIngr x |> Map.forall (konst <| fun name -> give |> List.exists (fst >> (=) name)) then on Prod2.getName Prod2.getPrice x :: st else st ) []

let convertToMaple give given recip =
    let m,prof = p recip give given |> List.mapi (fun i (name, x) -> sprintf "x%d" i |> fun y -> (name, y), (y, x)) |> List.unzip |> mapFst Map.ofList
    let xs = awer given recip |> Map.toList |> List.map (snd >> mapFst (List.choose (fun (x,y) -> Map.tryFind x m |> Option.map (flip comma y)))) |> List.filter (fst >> List.isEmpty >> not)
    let fn = List.map (curry (flip <| sprintf "%d*%s")) >> String.concat " + "
    let prof' = prof |> fn
    [xs |> List.map (mapFst fn >> curry (sprintf "%s <= %d")) |> String.concat ", "; prof'] |> fun xs -> System.IO.File.WriteAllLines(@"e:\forMaple.txt", xs)
    m

let readFromMaple (reciples:ReciplesType) m =
    let asd =
        let s = System.IO.File.ReadAllText @"e:\fromMaple.txt"
        s |> fun x -> System.Text.RegularExpressions.Regex.Matches(x, "(x\d+) = (\d+)") |>  Seq.cast<System.Text.RegularExpressions.Match> |> Seq.map (fun x -> if x.Success then x.Groups |> fun x -> x.[1].Value, int x.[2].Value  else failwithf "%A" x) |> List.ofSeq
    let unterpm = m |> Map.toList |> List.map (curry <| flip comma) |> Map.ofList // |> ( List.map << flip Map.find)
    asd |> List.map (mapFst <| flip Map.find unterpm) |> List.filter (snd >> ((<>) 0))

let give reciples =
    System.IO.File.ReadAllLines(@"e:\currstate.txt", System.Text.Encoding.GetEncoding "windows-1251") |> List.ofArray |> List.map (FsharpMyExtension.Show.split '\t' >> function [|name; count|] -> name, (System.Text.RegularExpressions.Regex.Matches(count, "\d+") |> cond (function null -> false | _ -> true) (Seq.cast<System.Text.RegularExpressions.Match> >> Seq.sumBy(fun x -> int x.Value)) (fun _ -> failwithf "%A" name)) | x -> failwithf "%A" x)
    //|> List.head |> fst |> flip Map.tryFind reciples
    |> fun xs -> xs |> List.map (fst >> s (fun x -> cond Option.isNone (fun _ -> printfn "%A" x; reciples |> Map.exists (fun _ v -> Prod2.getIngr v |> Map.exists (konst ((=) x))) |> cond id (konst id ()) (fun _ -> failwithf "not found %A" x)) (konst id ())) (flip Map.tryFind reciples)) |> ignore; xs

let given give = give |> List.map (mapSnd (fun c -> [], c)) |> Map.ofList

let res () =
    let lst = System.IO.File.ReadAllText @"e:\Project\ExpertSystem\ExpertSystem\bin\Debug\bd.dat" |> fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<ReciplesType> x
    let reciples' = reciples lst
    //(Map.map <| konst id Prod2.getProd) reciples' |> flip expand ("картофельная решетка", 1)
    let recip = recipWithPrices reciples'

    let m =
        give reciples' |> fun give ->
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

    reciples lst |> Map.fold (fun st k -> Prod2.getIngr >> Map.toSeq >> Map.reducef fst (fun _ v -> k::v) [] st) Map.empty
//let given = give |> List.map (mapSnd (fun c -> [], c)) |> Map.ofList

let xs = id >> (printf "eval..."; id) : 'a -> int

let f x = if x = 10 then printf "eval true..."; x else printf "eval false..."; x
let f' = cond ((=) 10) (printf "eval true..."; id) (konts id (printf "eval false..."))