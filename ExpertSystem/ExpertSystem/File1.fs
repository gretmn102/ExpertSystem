module File1

#if INTERACTIVE
#load "expander.fs"
//#r @"..\..\..\RecipleInputForm\bin\Debug\RecipleInputForm.dll"
#endif

open FsharpMyExtension.Tree
open FsharpMyExtension.Map
open FsharpMyExtension.FSharpExt
open FsharpMyExtension.Option
open FsharpMyExtension.List
type ReciplesType = (string * (int * (string * int) list)) list
type 'a T = { Name:string; ProdCount:int; Ingr:(string*int) list; Price:'a }

let expand reciples = 
    let rec expa last ((name, count) as curr) = 
        let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty

        let p, ingrs = 
            let { ProdCount = p; Ingr = ingrs } = Map.find name reciples // crafts.[name]
            let coeff = (float count/ float p) |> ceil |> int
            coeff*p, ingrs |> List.map (function n, c -> n, coeff * c)
        if valid last (List.map fst ingrs) then
                Tree.Node((name, p), List.map (expa (name::last)) ingrs)
        else Tree.Node(curr, [])
    expa []
assert
    [ { Name = "a"; ProdCount = 1; Ingr = ["b", 1]; Price = 0 }
      { Name = "b"; ProdCount = 1; Ingr = ["c", 2]; Price = 0 }
      { Name = "c"; ProdCount = 1; Ingr = []; Price = 0 } ] |> Seq.map (fun x -> x.Name, x) |> Map.ofSeq |> flip expand ("a", 1)
    true

let lst = System.IO.File.ReadAllText @"e:\Project\ExpertSystem\ExpertSystem\bin\Debug\bd.dat" |> fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<ReciplesType> x

//lst |> Map.ofList |> Map.filter (fun _ -> snd >> List.exists (fst >> (=) "gold") >> not) |> Seq.iter (printfn "%A")

let reciples =
    lst |> List.map (fun (x, (count, xs)) -> List.partition (fst >> (=) "gold") xs |> (function [], _ -> None, (x, (count, xs)) | [_,n], xs -> Some n, (x, (count, xs)) | x -> failwithf "many golds:\n%A" x))//  fun xs -> 
    //|> List.fold (fun (gold, recip) (g, (name,body)) -> Map.add name g gold, Map.add name body recip ) (Map.empty, Map.empty)
    |> List.fold (fun st (g, (name,(count, ingr))) -> Map.add name { Name = name; Price = g; ProdCount = count; Ingr = ingr } st ) Map.empty

//        |> fun m -> Map.choose (fun _ x -> x.Price |> Option.bind (fun p -> if x.Ingr |> List.forall (fun (name, _ ) -> Map.find name m |> fun x -> Option.isSome x.Price) then Some { Name = x.Name; Price = p; ProdCount = x.ProdCount; Ingr = x.Ingr; } else None ) ) m
let remNotPrice m = Map.choose (fun _ x -> x.Price |> Option.bind (fun p -> if x.Ingr |> List.forall (fun (name, _ ) -> Map.find name m |> fun x -> Option.isSome x.Price) then Some { Name = x.Name; Price = p; ProdCount = x.ProdCount; Ingr = x.Ingr; } else None ) ) m
let print xs = Seq.iter (printfn "%A") xs
    //|> Map.fold (fun st k v -> match v.Price with Some p -> st | None -> st ) Map.empty
(*    let res name =
    let xs = expand reciples (name, 1)
    match Map.find name golds with
    | Some sell ->
        let sourcePrice = Tree.leafs xs |> List.map (fun (x, count) -> Map.find x golds |> function Some x -> x * count | None -> 0 ) |> List.sum
        (name, sourcePrice, sell, sell - sourcePrice) |> Some
    | None -> None 
    
Map.toList golds |> List.choose (fst >> res >> function Some(_,_,_,0) -> None | x -> x)
|> List.sortBy (function _,_,_,x -> x)
|> print  *)
let recip =
    reciples |> Map.map (fun name x -> Map.find name reciples |> fun x -> expand reciples (name, x.ProdCount) |> fun t -> Tree.leafs t |> fun xs -> { x with Ingr = xs; ProdCount = Tree.getValue t |> snd }) |> remNotPrice

let res =
    recip |> Seq.map (fun (KeyValue(_, x)) -> x.Ingr |> List.map (fun (x,count) -> Map.find x recip |> fun x -> x.Price * count) |> List.sum |> fun sourcePrice -> (x.Name, x.Price, sourcePrice * x.ProdCount, x.Price - sourcePrice, x.Ingr ))
res |> Seq.sortBy (function _,_,_,x,_ -> x) |> print

let give =
    System.IO.File.ReadAllLines(@"e:\currstate.txt", System.Text.Encoding.GetEncoding "windows-1251") |> List.ofArray |> List.map (FsharpMyExtension.Show.split '\t' >> function [|name; count|] -> name, (System.Text.RegularExpressions.Regex.Matches(count, "\d+") |> cond (function null -> false | _ -> true) (Seq.cast<System.Text.RegularExpressions.Match> >> Seq.sumBy(fun x -> int x.Value)) (fun _ -> failwithf "%A" name)) | x -> failwithf "%A" x)
    //|> List.head |> fst |> flip Map.tryFind reciples
    |> fun xs -> xs |> List.map (fst >> s (fun x -> cond Option.isNone (fun _ -> printfn "%A" x; reciples |> Map.exists (fun _ v -> v.Ingr |> List.exists (fst >> ((=) x))) |> cond id (konst id ()) (fun _ -> failwithf "not found %A" x)) (konst id ())) (flip Map.tryFind reciples)) |> ignore; xs
    
(*         [("какао-стручок", 19); ("рис", 102); ("сахар", 22); ("пшеница", 7);
                ("кофейные зерна", 2); ("морковь", 2); ("кукуруза", 4); ("яйцо", 2);
                ("молоко", 2); ("жемчужный горох", 2); ("картофель", 17); ("томат", 4); ("сочносливка", 5); "ананас", 1] *)
            //[("какао-стручок", 19); ("рис", 102);]
    

let given = give |> List.map (mapSnd (fun c -> [], c)) |> Map.ofList
let inReciple x = reciples |> Map.filter (fun _ v -> v.Ingr |> List.exists (fst >> ((=) x)))

let awer () =
    recip |> Map.fold (fun st _ x -> if Map.containsKey x.Name given then st else x.Ingr |> List.fold (fun st (name, count) -> Map.tryFind name st |> Option.map (mapFst (fun xs -> (x.Name,count)::xs) >> fun x -> Map.add name x st) |> Option.getOrDef (fun () -> st) ) st )  given
let p = recip |> Map.fold (fun st _ x -> if Map.containsKey x.Name given then st else if x.Ingr |> List.forall (fst >> fun name -> give |> List.exists (fst >> (=)name)) then (x.Name, x.Price)::st else st ) []

let convertToMaple () =
    let m,prof = p |> List.mapi (fun i (name, x) -> sprintf "x%d" i |> fun y -> (name, y), (y, x)) |> List.unzip |> mapFst Map.ofList
    let xs = awer() |> Map.toList |> List.map (snd >> mapFst (List.choose (fun (x,y) -> Map.tryFind x m |> Option.map (flip comma y)))) |> List.filter (fst >> List.isEmpty >> not)
    let fn = List.map (curry (flip <| sprintf "%d*%s")) >> String.concat " + "
    let prof' = prof |> fn
    [xs |> List.map (mapFst fn >> curry (sprintf "%s <= %d")) |> String.concat ", "; prof'] |> fun xs -> System.IO.File.WriteAllLines(@"e:\forMaple.txt", xs)
    m
let profitEval = List.map (fun (x, count) -> Map.find x recip |> fun y -> y.Price * count ) >> List.sum

let readFromMaple m =
    let asd =
        let s = System.IO.File.ReadAllText @"e:\fromMaple.txt"
        s |> fun x -> System.Text.RegularExpressions.Regex.Matches(x, "(x\d+) = (\d+)") |>  Seq.cast<System.Text.RegularExpressions.Match> |> Seq.map (fun x -> if x.Success then x.Groups |> fun x -> x.[1].Value, int x.[2].Value  else failwithf "%A" x) |> List.ofSeq
    let unterpm = m |> Map.toList |> List.map (curry <| flip comma) |> Map.ofList // |> ( List.map << flip Map.find)
    let dones = asd |> List.map (mapFst <| flip Map.find unterpm) |> List.filter (snd >> ((<>) 0))
    Expander.expand2Start (Map.ofList lst |> Map.add "крафт" (1, dones)) Map.empty ("крафт", 1)

let m = convertToMaple ()
readFromMaple m |> printfn "%A"

module MySolve =
    let xs =
        awer() |> Map.toList |> List.map snd |> List.mapi (fun i -> mapFst (fun xs -> (sprintf "x%i" i, 1)::xs))
        |> List.map (mapFst Map.ofList)
    let len = List.length xs
    let pp = List.init len (on <| sprintf "x%i" <| fun _ -> 0) @ p
    let ss = pp |> List.fold (fun st -> fst >> fun x -> xs |> List.map (fst >> Map.tryFind x >> Option.getOrDef (fun () -> 0)  ) |> fun ys -> (x, ys)::st ) []
    let ss' = ss |> List.map snd |> List.trans |> List.map2 (fun x y -> [0] @ y @ [snd x]) give
    let pp' = pp |> List.map (snd >> (~-)) |> List.rev |> fun x -> [1] @ x @ [0]
    let res2 = ss' @ [pp']
    res2
    ss
    p
    xs
    //Map.fold (fun st k x -> if)

    let meth xss =
        let split n k (xs:_ []) = xs.[n..k]
        let last xs = (s Array.get (Array.length >> (flip (-) 1))) xs
        let smallInProfit xs = (last >> Seq.mapi comma >> Seq.minBy snd >> fst) xs
        let inline smallestRow j = s (Array.map last >> Array.map2 (/)) <| Array.map (flip Array.get j) >> s (flip <| split 0) (Array.length >> (flip (-) 2)) >> Seq.mapi comma >> Seq.minBy snd >> fst
        //[|1..10|] |> fun xs -> xs.[0..2]
        let inline neg x = -x
        let inline basis i j =
            let f = flip Array.get i >> s (fun xs n -> Array.map (flip (/) n) xs ) (flip Array.get j)
            s (Array.copy >> (fun xss ys -> (flip Array.set i) xss ys; xss |> Array.iteri (fun i' x -> if i = i' then () else x |> flip Array.get j |> neg |> fun koeff -> Array.map2 ((*) koeff >> (+)) (Array.get xss i) x |> Array.set xss i' ); xss) ) f
        let fn (xss:float [] []) =  smallInProfit xss |> fun j -> smallestRow j xss |> fun i -> basis i j xss
        fn xss
    [
        [0; 1; 1; 1; 0; 0; 100]
        [0; 6; 3; 0; 1; 0; 360]
        [0; 1; 2; 0; 0; 1; 120]
        [1;-2;-3; 0; 0; 0; 0 ]
    ] |> List.map (List.map float >> Array.ofList) |> Array.ofList |> meth
    
    //recip |> Seq.map (fun (KeyValue(k, x)) -> x.Ingr   ))
    [
        [0;   2;   3; 1; 0;  0; 50]
        [0;   3;   0; 0; 1;  0; 30]
        [0;   0;   4; 0; 0;  1; 70]
        [1; -30; -50; 0; 0;  0;  0]
    ] |> List.map (List.map float >> Array.ofList) |> Array.ofList |> meth
    res2 |> List.map (List.map float >> Array.ofList) |> Array.ofList |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth |> meth
    res2
    ss
    //reciples |> Seq.map (fun (KeyValue(k, _)) -> res k ) |> Seq.nth 36 // |> Seq.sortBy (function _,_,_,x -> x) |> print
    Seq.nth 36 reciples
    reciples.["шоколад"]
    reciples.["какао-стручок"]

