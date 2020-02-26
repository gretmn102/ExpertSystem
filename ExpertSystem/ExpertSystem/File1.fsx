#I @"e:\Project\FsharpMyExtension\FsharpMyExtension\FsharpMyExtension\bin\Debug\net461\"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
#r @"Fuchu.dll"
#r @"HtmlAgilityPack.dll"
#r @"Newtonsoft.Json.dll"
#r @"Newtonsoft.Json.Bson.dll"
#r @"FsharpMyExtension.dll"

#if INTERACTIVE
#load "Expander.fs"
#r @"..\..\ExpertSystem\RecipleInputForm\bin\Debug\RecipleInputForm.dll"
#load "Program.fs"
#endif

open FsharpMyExtension
open FsharpMyExtension.Tree

// Есть куча всякой всячины, и это всё можно сочетать между собой по заданным рецептам и получать новые вещи. Также у каждой вещи — фиксированная цена. Вопрос: как всё это добро скомбинировать, чтобы получить как можно больше прибыли?
// Для решения используется симплекс-метод.

// Как это решить в лоб?
// 1. Берем все предметы и считаем их общую цену.
// 2. Отсеиваем, что можем из них создать.
// 3. Берем первый попавшийся и создаем.
// ...

// Как можно уменьшить заведомо убыточные рецепты? Сложить цены всех составляющих и сравнить с ценой итогового продукта.

// Первоначально задача ставилась для Starbound и звучала так: есть куча еды, из которой можно изготовить много всякого и продать за какую-то цену. Вопрос: что из этого нужно изготовить, чтобы извлечь как можно больше прибыли?

type Cost = int

module BruteSolve =
    open Expander
    let splitCost costName (reciplesDb:Program.Reciples) =
        reciplesDb
        |> Map.mapFold (fun costs name reciple ->
            let costs', rests =
                List.partition (fst >> (=) costName) reciple.Ingredients
            let costs =
                match costs' with
                | [_, cost] -> Map.add name (cost:Cost) costs
                | [] -> costs
                | xs -> failwithf "many items with '%s':\n%A" costName xs

            let reciple = { reciple with Ingredients = rests }
            reciple, costs) Map.empty
        |> mapFst (fun x -> Map.remove costName x)

    let brute reciples (costs:Map<ItemName,int>) (stock:Map<ItemName, HaveInStock>) =
        let rec loop (stock:Map<ItemName, HaveInStock>) =
            let rec remFromStock (stock:Map<ItemName, HaveInStock>) = function
                | (name, count)::xs ->
                    match Map.tryFind name stock with
                    | Some haveInStock ->
                        let diff = haveInStock - count
                        if diff < 0 then None
                        else
                            let stock = Map.add name diff stock
                            remFromStock stock xs
                    | None -> None
                | [] -> Some stock

            reciples
            |> Map.toList
            |> List.choose (fun (_, reciple) ->
                if List.isEmpty reciple.Ingredients then None
                else
                    match remFromStock stock reciple.Ingredients with
                    | Some stock ->
                        stock
                        |> Map.addOrMod reciple.ItemName reciple.OutputCount
                            ((+) reciple.OutputCount)
                        |> fun stock ->
                            let profit = 
                                stock
                                |> Map.fold (fun acc name inStock ->
                                    match Map.tryFind name costs with
                                    | Some cost -> 
                                        cost * inStock + acc
                                    | None ->
                                        // printfn "%A" name
                                        acc
                                        ) 0
                            Node((reciple.ItemName, profit), loop stock)
                            |> Some
                    | None -> None
            )
        loop stock
    let test () =
        let reciples, costs =
            let reciplesDb =
                let path = @"Info\OldDBs\starbound.json"
                // let path = @"ExpertSystem\ExpertSystem\bin\Debug\net461\bd.json"
                let db:Program.Reciples = Json.desf path
                db
            splitCost "gold" reciplesDb
        let inputExample =
            [
            //  ("какао-стручок", 19);
            //  ("рис", 102);
            //  ("сахар", 22);
            //  ("пшеница", 7);
             ("кофейные зерна", 2);
             ("морковь", 2);
            // ("кукуруза", 4); ("яйцо", 2);
            //  ("молоко", 2); ("жемчужный горох", 2); ("картофель", 17); ("томат", 4);
            //  ("сочносливка", 5); ("ананас", 1)
            ]
            |> Map.ofList
        let res = brute reciples costs inputExample
        Node(("", 0), res) |> Tree.visualize (sprintf "%A")
        res |> Tree.unpack

type ProductName = string
type ProductIngr = ProductName * int
type 'Price Reciple =
    {
        Name:ProductName
        ProdCount:int
        Ingrs: ProductIngr list
        Price:'Price
    }

let expand reciples =
    let rec expa last ((name, count) as curr) =
        let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty

        let p, ingrs =
            let { ProdCount = p; Ingrs = ingrs } = Map.find name reciples // crafts.[name]
            let coeff = (float count/ float p) |> ceil |> int
            coeff * p, ingrs |> List.map (function n, c -> n, coeff * c)
        if valid last (List.map fst ingrs) then
            Tree.Node((name, p), List.map (expa (name::last)) ingrs)
        else Tree.Node(curr, [])
    expa []
assert
    [
        { Name = "a"; ProdCount = 1; Ingrs = ["b", 1]; Price = 0 }
        { Name = "b"; ProdCount = 1; Ingrs = ["c", 2]; Price = 0 }
        { Name = "c"; ProdCount = 1; Ingrs = []; Price = 0 }
    ]
    |> Seq.map (fun x -> x.Name, x)
    |> Map.ofSeq
    |> flip expand ("a", 1)
    true

let toNewReciples costName (reciplesDb:Program.Reciples) =
    reciplesDb
    |> Map.toList
    |> List.map (fun (name, reciple) ->
        let count, xs = reciple.OutputCount, reciple.Ingredients
        List.partition (fst >> (=) costName) xs
        |> (function
            | [], _ -> None, (name, (count, xs))
            | [_,n], xs -> Some n, (name, (count, xs))
            | x -> failwithf "many items with '%s':\n%A" costName x))
    |> List.fold (fun st (g, (name,(count, ingr))) ->
        let v = { Name = name; Price = g; ProdCount = count; Ingrs = ingr }
        Map.add name v st ) Map.empty

let reciplesDb =
    let path = @"Info\OldDBs\starbound.json"
    // let path = @"ExpertSystem\ExpertSystem\bin\Debug\net461\bd.json"
    let db:Program.Reciples = Json.desf path
    db
let newReciples = toNewReciples "gold" reciplesDb

let print xs =
    Seq.map (sprintf "%A") xs
    |> String.concat "\n"

let recip =
    let remNotPrice m =
        Map.choose (fun _ x ->
            x.Price
            |> Option.map (fun p ->
                // let isValid =
                //     x.Ingr
                //     |> List.forall (fun (name, _) ->
                //         Map.find name m
                //         |> fun x -> Option.isSome x.Price)
                // if isValid then
                    {
                        Name = x.Name
                        Price = p
                        ProdCount = x.ProdCount
                        Ingrs = x.Ingrs
                    }
                    // |> Some
                // else None
                ) ) m
    newReciples
    |> Map.map (fun name x ->
        Map.find name newReciples
        |> fun x -> expand newReciples (name, x.ProdCount)
        |> fun t ->
            Tree.leafs t
            |> fun xs -> { x with Ingrs = xs; ProdCount = Tree.getValue t |> snd })
    |> remNotPrice
// reciples |> print |> Clipboard.setText
// recip  |> print |> Clipboard.setText



let getInput inputPath =
    System.IO.File.ReadAllLines inputPath
    |> List.ofArray
    |> List.map (
        FsharpMyExtension.Show.split '\t'
        >> function
            | [|name; count|] ->
                let count =
                    System.Text.RegularExpressions.Regex.Matches(count, "\d+")
                    |> cond
                        (function null -> false | _ -> true) (Seq.cast<System.Text.RegularExpressions.Match>
                         >> Seq.sumBy(fun x -> int x.Value))
                        (fun _ -> failwithf "%A" name)
                (name:ProductName), (count:Expander.HaveInStock)
            | x -> failwithf "%A" x)
    //|> List.head |> fst |> flip Map.tryFind reciples
    |> fun xs ->
        xs
        |> List.iter (
            fst
            >> s (fun x ->
                    cond
                        Option.isNone
                        (fun _ ->
                            printfn "%A" x
                            newReciples
                            |> Map.exists (fun _ v ->
                                v.Ingrs
                                |> List.exists (fst >> ((=) x)))
                            |> cond id (konst id ())
                                (fun _ -> failwithf "not found %A" x))
                        (konst id ()))
                    (flip Map.tryFind newReciples))
        xs

let inputPath = @"Info\Maple\input.txt"
/// Что будет, если сюда запихнуть что-нибудь готовое? Например, "картофельное пюре"? Есть надежда, что скрипт посчитает, что из него можно сделать "картофельную решетку". Да, по цене получается одинаково, но всё же.
let inputExample =
    [("какао-стручок", 19); ("рис", 102); ("сахар", 22); ("пшеница", 7);
     ("кофейные зерна", 2); ("морковь", 2); ("кукуруза", 4); ("яйцо", 2);
     ("молоко", 2); ("жемчужный горох", 2); ("картофель", 17); ("томат", 4);
     ("сочносливка", 5); ("ананас", 1)]
let input : (ProductName * Expander.HaveInStock) list =
    getInput inputPath
    // [ "e", 3; "f", 10; "c", 20 ]
type ProductCost = int
let constraints, profits =
    let given =
        input
        |> List.map (mapSnd (fun c -> [], c))
        |> Map.ofList
    recip
    |> Map.fold (fun ((constraints, profits) as st) _ reciple ->
        if Map.containsKey reciple.Name given then st
        else
            let profits =
                let isValid =
                    reciple.Ingrs
                    |> List.forall (
                        fst
                        >> fun name -> input |> List.exists (fst >> (=) name))
                if isValid then
                    ((reciple.Name:ProductName), (reciple.Price:ProductCost))::profits
                else profits
            let constraints =
                reciple.Ingrs
                |> List.fold (fun st (name, count) ->
                    Map.tryFind name st
                    |> Option.map (
                        mapFst (fun xs ->
                            ((reciple.Name, count):ProductIngr)::xs)
                        >> fun x -> Map.add name x st)
                    |> Option.defaultValue st ) constraints
            constraints, profits
        ) (given, [])

type VarName = string
let toMaplePath = @"Info\Maple\toMaple.txt"
let ofMaplePath = @"Info\Maple\ofMaple.txt"
let convertToMaple () =
    let m, profits =
        profits
        |> List.mapi (fun i (name, count) ->
            sprintf "x%d" i
            |> fun var -> (name, (var:VarName)), ((var:VarName), count))
        |> List.unzip
        |> mapFst Map.ofList
    let constraints =
        constraints
        |> Map.toList
        |> List.map (
            snd
            >> mapFst (
                List.choose (fun (x,y) ->
                    Map.tryFind x m
                    |> Option.map (flip comma y))
            ))
        |> List.filter (fst >> List.isEmpty >> not)
    let f = List.map (curry (flip <| sprintf "%d*%s")) >> String.concat " + "
    [
        constraints
        |> List.map (mapFst f >> curry (sprintf "%s <= %d"))
        |> String.concat ", "

        f profits
    ]
    |> uncurry System.IO.File.WriteAllLines toMaplePath
    m
let readFromMaple (m:Map<ProductName, VarName>) =
    let m =
        m |> Map.toList
        |> List.map (curry <| flip comma)
        |> Map.ofList
    System.IO.File.ReadAllText ofMaplePath
    |> fun x ->
        System.Text.RegularExpressions.Regex.Matches(x, "(x\d+) = (\d+)")
        |> Seq.cast<System.Text.RegularExpressions.Match>
        |> Seq.map (fun x ->
            if x.Success then
                x.Groups
                |> fun x -> (x.[1].Value : VarName), int x.[2].Value
            else failwithf "%A" x)
        |> List.ofSeq
    |> List.map (mapFst <| flip Map.find m)
    |> List.filter (snd >> ((<>) 0))

let expand3 (xs:(ProductName * _) list) =
    let reciples =
        let reciple =
            {
                Expander.Ingredients = xs
                Expander.ItemName = "крафт"
                Expander.OutputCount = 1
            }
        Map.add "крафт" reciple reciplesDb
        // |> Expander.Reciples
    Expander.expand2Start reciples Map.empty ("крафт", 1)

let m = convertToMaple ()
// m |> Map.toList |> sprintf "%A" |> Clipboard.setText
let test () =
    let m =
        [("ананасовый джем", "x76"); ("ананасовый сок", "x75"); ("банан в кляре", "x74");
         ("банановое мороженое", "x73"); ("банановый пирог со сливками", "x72");
         ("банановый пончик со сливками", "x71"); ("блинчики из жемчуга", "x70");
         ("бон бон", "x69"); ("бон бон бон бон", "x68"); ("вареный жемчуг", "x67");
         ("вареный рис", "x66"); ("вареный томат", "x65"); ("вершкоперовый джем", "x64");
         ("гамбургер", "x63"); ("джем из жемчужного гороха", "x62");
         ("джем из киви", "x61"); ("жареная морковь", "x60"); ("жареная птица", "x59");
         ("жареный банан", "x58"); ("жареный картофель", "x57");
         ("жареный стейк", "x56"); ("жаркое", "x55"); ("жемчужное ризотто", "x54");
         ("засахаренная кукуруза", "x53"); ("кактусовое мороженое", "x52");
         ("кактусовый сок", "x51"); ("картофельная решетка", "x50");
         ("картофельное пюре", "x49"); ("клювниковый джем", "x48");
         ("клювниковый пирог", "x47"); ("клювниковый хлеб", "x46"); ("клюворуза", "x45");
         ("колючее печенье", "x44"); ("кофе", "x43"); ("кофе с молоком", "x42");
         ("кофейный торт", "x41"); ("кровавая кокетка", "x40");
         ("кукурузный початок", "x39"); ("мокаччино", "x38"); ("морковный сок", "x37");
         ("морковный суп", "x36"); ("морковный торт", "x35"); ("морковный хлеб", "x34");
         ("мясные пельмени", "x33"); ("овсянка", "x32"); ("огородный салат", "x31");
         ("оладьи алексов", "x30"); ("оперенный каравай", "x29");
         ("пастуший пирог", "x28"); ("пирожок", "x27"); ("попкорн", "x26");
         ("похлебка из жемчужного гороха", "x25"); ("приправа", "x24");
         ("рисовый пирог", "x23"); ("рисовый пирог с гарниром", "x22");
         ("сладкие гренки", "x21"); ("сладкий попкорн", "x20"); ("снежный рожок", "x19");
         ("сок киви", "x18"); ("сочносливочная запеканка", "x17");
         ("сочносливочный джем", "x16"); ("сочносливочный пирог", "x15");
         ("счастливый соус", "x14"); ("сыр", "x13"); ("томатный сок", "x12");
         ("томатный суп", "x11"); ("торт", "x10"); ("тост", "x9"); ("тушье мясо", "x8");
         ("фаршированный томат", "x7"); ("хлеб", "x6"); ("чизбургер", "x5");
         ("шипофруктовый сок", "x4"); ("шоколад", "x3"); ("шоколадный торт", "x2");
         ("ядовитый пончик", "x1"); ("ядолистовый джем", "x0")]
    let fromMapl =
        [("ядолистовый джем", 2); ("томатный сок", 28); ("сок киви", 3);
         ("сладкий попкорн", 16); ("сладкие гренки", 14);
         ("рисовый пирог с гарниром", 32); ("шоколад", 22); ("морковный хлеб", 4);
         ("морковный сок", 40); ("мокаччино", 28); ("кровавая кокетка", 22);
         ("клювниковый хлеб", 12); ("картофельное пюре", 49); ("кактусовый сок", 12);
         ("жемчужное ризотто", 19); ("жареный банан", 7); ("вершкоперовый джем", 2);
         ("вареный рис", 266); ("фаршированный томат", 2); ("ананасовый сок", 3)]
    ()
let result = readFromMaple m
result |> expand3 |> printfn "%A"
// readFromMaple m |> sprintf "%A" |> Clipboard.setText

let profitEval =
    List.sumBy (fun ((name:ProductName), count) ->
        let y = Map.find name newReciples
        y.Price
        |> Option.map ((*) count)
        |> Option.defaultValue 0
    )
// profitEval result

/// TODO: воплотить функцию `simplex:-maximize` из Maple, чтобы избавиться от него.
/// Насколько мне помнится, есть хорошая лекция по линейному программированию, где хорошо объясняется, как это делать. Вроде бы, нужно решить матрицу.
module MySolve =
    let xs =
        constraints
        |> Map.toList
        |> List.map snd
        |> List.mapi (fun i ->
            mapFst (fun xs -> (sprintf "x%i" i, 1)::xs))
        |> List.map (mapFst Map.ofList)
    let len = List.length xs
    let pp = List.init len (on <| sprintf "x%i" <| fun _ -> 0) @ profits
    let ss =
        pp
        |> List.fold (fun st ->
            fst
            >> fun x ->
                xs
                |> List.map (
                    fst
                    >> Map.tryFind x
                    >> Option.defaultWith (fun () -> 0))
                |> fun ys -> (x, ys)::st ) []
    let ss' =
        ss
        |> List.map snd
        |> List.trans
        |> List.map2 (fun x y ->
            [ yield 0; yield! y; yield snd x ]
           ) input
    let pp' =
        pp
        |> List.map (snd >> (~-))
        |> List.rev
        |> fun x ->
            [yield 1; yield! x; yield 0]
    let res2 = ss' @ [pp']
    res2
    ss
    profits
    xs
    //Map.fold (fun st k x -> if)

    let simplexMaximize xss =
        let split n k (xs:_ []) = xs.[n..k]
        let last xs = (s Array.get (Array.length >> (flip (-) 1))) xs
        let smallInProfit xs =
            (last >> Seq.mapi comma >> Seq.minBy snd >> fst) xs
        let inline smallestRow j =
            s (Array.map last >> Array.map2 (/))
            <| Array.map (flip Array.get j)
            >> s (flip <| split 0) (Array.length >> (flip (-) 2))
            >> Seq.mapi comma
            >> Seq.minBy snd
            >> fst
        //[|1..10|] |> fun xs -> xs.[0..2]
        let inline neg x = -x
        let inline basis i j =
            let f =
                flip Array.get i
                >> s (fun xs n -> Array.map (flip (/) n) xs ) (flip Array.get j)
            s (Array.copy >> (fun xss ys -> (flip Array.set i) xss ys; xss |> Array.iteri (fun i' x -> if i = i' then () else x |> flip Array.get j |> neg |> fun koeff -> Array.map2 ((*) koeff >> (+)) (Array.get xss i) x |> Array.set xss i' ); xss) ) f
        let fn (xss:float [] []) =
            smallInProfit xss
            |> fun j ->
                smallestRow j xss |> fun i -> basis i j xss
        fn xss
    [
        [0; 1; 1; 1; 0; 0; 100]
        [0; 6; 3; 0; 1; 0; 360]
        [0; 1; 2; 0; 0; 1; 120]
        [1;-2;-3; 0; 0; 0; 0  ]
    ] |> List.map (List.map float >> Array.ofList) |> Array.ofList |> simplexMaximize

    //recip |> Seq.map (fun (KeyValue(k, x)) -> x.Ingr   ))
    [
        [0;   2;   3; 1; 0;  0; 50]
        [0;   3;   0; 0; 1;  0; 30]
        [0;   0;   4; 0; 0;  1; 70]
        [1; -30; -50; 0; 0;  0;  0]
    ] |> List.map (List.map float >> Array.ofList) |> Array.ofList |> simplexMaximize
    res2 |> List.map (List.map float >> Array.ofList) |> Array.ofList |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize |> simplexMaximize
    res2
    ss
    //reciples |> Seq.map (fun (KeyValue(k, _)) -> res k ) |> Seq.nth 36 // |> Seq.sortBy (function _,_,_,x -> x) |> print
    Seq.nth 36 newReciples
    newReciples.["шоколад"]
    newReciples.["какао-стручок"]
