module Expander
open FsharpMyExtension.Tree

let expand reciples = 
    let rec expa last ((name, count) as curr) = 
        let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty

        let p, ingrs = 
            let p, ingrs = Map.find name reciples // crafts.[name]
            let coeff = (float count/ float p) |> ceil |> int
            coeff*p, ingrs |> List.map (function n, c -> n, coeff * c)
        if valid last (List.map fst ingrs) then
                Tree.Node((name, p), List.map (expa (name::last)) ingrs)
        else Tree.Node(curr, [])
    expa []
/// <summary>
/// Последовательно собирает предметы.
/// </summary>
/// <param name="reciples"></param>
/// <param name="resources"></param>
/// <param name="lst"></param>
let assemble reciples resources lst = 
    let f state (name, count) =
        let f state (name, count) = 
            match Map.tryFind name state with
            | None -> failwithf "%A not found in %A" name state
            | Some count' ->
                let diff = count' - count
                if diff < 0 then failwithf "%A - %A = %d" (name, count') count diff
                elif diff = 0 then Map.remove name state
                else Map.add name diff state

        let craft state (name, count) = 
            let removes x y = List.fold f x y
                
            assert
                let state = [("d", 24); ("e", 66); ("f", 22)] |> Map.ofList
                let ingrs = [("e", 18); ("f", 6)]
                removes state ingrs = Map [("d", 24); ("e", 48); ("f", 16)]
            
            let p, ingrs = 
                let p, ingrs = Map.find name reciples
                p*count, ingrs |> List.map (function n, c -> n, count * c)

            let xs = removes state ingrs
            let v = match Map.tryFind name xs with None -> 0 | Some n -> n
            Map.add name (p + v) xs
        craft state (name, count)
    List.fold f resources lst
let expandNotMod reciples = 
    let rec expa last (name, count) = 
        let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty

        let p, ingrs = Map.find name reciples
        let reciple = (name, p, count)
        if valid last (List.map fst ingrs) then
                Tree.Node(reciple, List.map (expa (name::last)) ingrs)
        else Tree.Node(reciple, [])
    expa []
module expand2 =
//let expand2 reciples resStart req =
    let give res (name, count) =
        match Map.tryFind name res with
        | None -> Map.add name count res
        | Some c -> Map.add name (c + count) res
    let get res (name, need) = 
        match Map.tryFind name res with
        | None -> 
            //printfn "%A" name
            res, need
        | Some count -> 
            let diff = count - need
            if diff < 0 then Map.add name 0 res, abs diff
            else Map.add name diff res, 0
    //let expandRes reciples res tree =
    let rec expand res = function
        | Tree.Node((name, _, need), []) ->
            //printfn "%A" x
            let res, rest = get res (name, need)
            res, Tree.Node((name, rest), [])
        | Tree.Node((name, p, need), ingrs) ->
            let res', need' = get res (name, need)
            
            let coeff = (float need'/ float p) |> ceil |> int
            //printfn "name = %A; coeff = %A need = %A p = %A need' = %A res = %A res' = %A" name coeff need p need' res res'
            //printfn "name = %s; p = %d" name coeff
            //printfn "%A" (name, p*coeff-need)
            //let (res, tree) = List.foldBack f ingrs (res, [])
            let (res, tree) =
                let f (res, rest) x  = let res, x = expand res x in res, x::rest
                let ingrs =
                    let f (Tree.Node((n, p, need), xs)) = Tree.Node((n, p, need*coeff), xs)
                    List.map f ingrs
                List.fold f (res', []) ingrs
            let n = 
                let n = p*coeff-need'
                //let n = p*coeff-need
                if n < 0 then 
                    printfn "name = %A" name
                    printfn "coeff = %A need = %A p = %A need' = %A res = %A res' = %A" coeff need p need' res res'
                    printfn "p*coeff-need = %A; n < 0" <| p*coeff-need'
                    printfn "p*coeff-need' = %A" <| p*coeff-need'
                    0 else n
            give res (name, n), Tree.Node((name, coeff), List.rev tree)
    assert
        let expnd = 
          Tree.Node (("craft", 1, 1),
             [Tree.Node (("iron casing", 2, 2),
                 [Tree.Node (("iron plate", 1, 1),
                     [Tree.Node (("iron", 1, 1),[]); Tree.Node (("forge hammer", 1, 1),[])]);
                  Tree.Node (("forge hammer", 1, 1),[])]);
              Tree.Node (("iron plate", 1, 1),
                 [Tree.Node (("iron", 1, 1),[]); Tree.Node (("forge hammer", 1, 1),[])])])

        Tree.notleafs expnd |> printfn "%A"
        expand (Map ["e",1]) expnd |> snd |> Tree.notleafs |> printfn "%A"
        true
        //expandNotMod reciples tree |> expand res
    (*
    let makes tree = 
        let rec subtreeRev = function
            | Tree.Node(x, xs) -> Tree.Node(x, List.rev xs |> List.map subtreeRev)
        //let tree = subtreeRev tree
        let raw = Tree.notleafs tree |> List.rev |> List.filter (snd >> ((<>)0))
        let xs = Seq.groupBy fst raw
        xs |> List.ofSeq |> List.map (fun (x, xs) -> x, Seq.sumBy snd xs)
        raw
    assert
        let tree =
           (Tree.Node (("a", 1),
              [Tree.Node (("f", 1),[Tree.Node (("e", 0),[Tree.Node (("g", 0),[])])]);
               Tree.Node (("f", 1),[Tree.Node (("e", 1),[Tree.Node (("g", 1),[])])])]))
        //let tree = subtreeRev tree
        Tree.visualize tree |> printfn "%s"
        //Tree.notleafs tree
        makes tree |> printfn "%A"
        true
    let resources tree = 
        let xs = 
            let leafs = Tree.leafs tree |> List.filter (snd >> ((<>)0))
            Seq.groupBy fst leafs
            |> Seq.map (fun (key, v) -> (key, Seq.sumBy snd v))
        xs |> List.ofSeq // Seq.fold (give) res
    *)

    let makes tree = 
        let f t = 
            let xs = Tree.leafs t |> Seq.groupBy fst |> List.ofSeq
            xs |> List.choose (fun (k, (v:seq<'a*int>)) ->
                let x = Seq.sumBy snd v
                if x = 0 then None else Some(k, x))
        let rec f' acc = function
            | Tree.Node(x, []) -> [x]::acc
            | t -> f' (f t::acc) (Tree.cutLeaf t)
        f' [] tree |> List.rev
    //let (res, t) = expandRes reciples resStart req
    //resources t, makes t
let expand2Start reciples res req =
    let x = expandNotMod reciples req
    let (_, t) = expand2.expand res x
    expand2.makes t
    (*
assert
    let req = ("a", 3)
    let startRes = Map["a", 2]
    let res, makes = expandEnd startRes req

    assemble res (makes.[0])
    |> (fun x -> assemble x (makes.[1]))
    |> (fun x -> assemble x (makes.[2]))
    |> (fun x -> assemble x (makes.[3]))
    |> (fun x -> assemble x (makes.[4]))
    |> (fun x -> assemble x (makes.[5]))
    false
    *)

