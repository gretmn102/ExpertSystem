module Expander
open FsharpMyExtension
open FsharpMyExtension.Tree

type ItemName = string
type Ingredient = ItemName * int
type Reciple =
    {
        Name:ItemName
        // То, сколько получается в итоге предметов, если всё сделать по рецепту.
        OutputCount:int
        Ingredients: Ingredient list
    }

[<Struct>]
type 'ItemId Reciples when 'ItemId : comparison =
    Reciples of Map<'ItemId, (int * ('ItemId * int) list)>
let transformToNewDb path =
    let oldDb:(string * (int * (string * int) list)) list = Json.desf path
    Reciples(Map.ofList oldDb)
    |> Json.serf path
// transformToNewDb @"Info\OldDBs\terraria.json"

module expand2 =
    let expandNotMod reciples =
        let rec expa last (name, count) =
            let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty

            let p, ingrs =
                Map.tryFind name reciples
                |> Option.defaultWith (fun () -> failwithf "reciple not found '%A'" name)
            let reciple = name, p, count
            if valid last (List.map fst ingrs) then
                    Tree.Node(reciple, List.map (expa (name::last)) ingrs)
            else Tree.Node(reciple, [])
        expa []
    let give (name, count) =
        Map.addOrMod name count ((+) count)
    let get (name, need) (stocks:Map<'ItemId, _>) =
        match Map.tryFind name stocks with
        | None -> stocks, need
        | Some count ->
            let diff = count - need
            if diff < 0 then
                Map.add name 0 stocks, abs diff
            else
                Map.add name diff stocks, 0
    let rec expand (stocks:Map<'ItemId, _>) = function
        | Node((name, _, need), []) ->
            get (name, need) stocks
            |> mapSnd (fun rest -> Tree.singleton (name, rest))
        | Node((name, p, need), ingrs) ->
            let stocks, need = get (name, need) stocks
            let coeff = (float need / float p) |> ceil |> int
            let n = p * coeff - need
            ingrs
            // |> List.map (fun (Node((n, p, need), xs)) ->
            //         Node((n, p, need*coeff), xs))
            // |> List.fold (fun (stocks, rest) x ->
            |> List.fold (fun (stocks, rest) (Node((n, p, need), xs)) ->
                    Node((n, p, need*coeff), xs)
                    |> expand stocks
                    |> mapSnd (fun x -> x::rest)
                    )
                (stocks, [])
            |> mapPair (give (name, n)) (fun tree -> Node((name, coeff), List.rev tree))
    // assert
    //     let expnd =
    //       Tree.Node (("craft", 1, 1),
    //          [Tree.Node (("iron casing", 2, 2),
    //              [Tree.Node (("iron plate", 1, 1),
    //                  [Tree.Node (("iron", 1, 1),[]); Tree.Node (("forge hammer", 1, 1),[])]);
    //               Tree.Node (("forge hammer", 1, 1),[])]);
    //           Tree.Node (("iron plate", 1, 1),
    //              [Tree.Node (("iron", 1, 1),[]); Tree.Node (("forge hammer", 1, 1),[])])])

    //     Tree.notleafs expnd |> printfn "%A"
    //     expand (Map ["e",1]) expnd |> snd |> Tree.notleafs |> printfn "%A"
    //     true

    let makes tree =
        let rec f' acc = function
            | Node(x, []) -> [x]::acc
            | t ->
                let f =
                    Tree.leafs
                    >> List.groupBy fst
                    >> List.choose (fun (k, v) ->
                        let x = Seq.sumBy snd v
                        if x = 0 then None else Some(k, x))
                f' (f t :: acc) (Tree.cutLeaf t)
        f' [] tree |> List.rev

let expand2Start (Reciples reciples:'ItemId Reciples) (stocks:Map<'ItemId,int>) req =
    expand2.expandNotMod reciples req
    |> expand2.expand stocks
    |> snd
    |> expand2.makes

/// Последовательно собирает предметы.
let assemble reciples (stocks:Map<'ItemId, _>) lst =
    let get (stocks:Map<'ItemId, _>) (name, count) =
        match Map.tryFind name stocks with
        | None -> failwithf "%A not found in %A" name stocks
        | Some count' ->
            let diff = count' - count
            if diff < 0 then failwithf "%A - %A = %d" (name, count') count diff
            // elif diff = 0 then Map.remove name stocks
            else Map.add name diff stocks
    let f stocks (name, count) =
        let p, ingrs =
            Map.find name reciples
            |> mapPair ((*) count) (List.map (mapSnd ((*)count)))

        List.fold get stocks ingrs
        |> Map.addOrMod name p ((+) p)
    List.fold f stocks lst

let expandWithTest reciples (stocks:Map<'ItemId,int>) req =
    // let reciples = Map.ofList sample
    // let stocks = Map.ofList ["compressor", 3]
    // let req = ("craftReactor", 1)
    let stocksFinal, tree =
        expand2.expandNotMod reciples req
        |> expand2.expand stocks
    
    let xss = expand2.makes tree
    let exp =
        List.fold (assemble reciples) stocks xss
    let act =
        stocksFinal
        |> Map.addOrMod (fst req) (snd req) ((+) (snd req))
    let f m = m |> Map.filter (fun _ -> (<>) 0)
    if f exp = f act then
        xss
    else
        failwith "not assemble"


