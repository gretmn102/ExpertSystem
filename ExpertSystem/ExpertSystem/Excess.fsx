#I @"e:\Project\FsharpMyExtension\FsharpMyExtension\FsharpMyExtension\bin\Debug\net461\"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
#r @"Fuchu.dll"
#r @"HtmlAgilityPack.dll"
#r @"Newtonsoft.Json.dll"
#r @"Newtonsoft.Json.Bson.dll"
#r @"FsharpMyExtension.dll"
open FsharpMyExtension
open FsharpMyExtension.Tree

module List = 
    let remove x xs = 
        let rec f acc = function
            | h::t -> if h = x then List.rev acc @ t else f (h::acc) t
            | [] -> failwithf "%A not found in %A" x xs
        f [] xs

module SimpleCase =
    let crafts = 
        //[Craft("a", ["b"; "c"; "g"]); Craft("b", ["c"; "d"]); Craft("c", ["e"; "f"]); Craft("d", ["e"; "f"]); Craft("g", ["d"])]
        // рекурсивная ссылка a -> b -> d -> a
        [("a", ["b";]); ("b", ["c"; "d"; "e"]); ("c", ["d"]); ("e", []); ("d", ["a"; "c"])]
        |> Map.ofList
    let expand = 
        let rec expa last name = 
            let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty
            assert valid ['a'; 'b'] ['c'; 'd'; 'a']

            let ingrs = crafts.[name]
            if valid last ingrs then
                Tree.Node(name, List.map (expa (name::last)) ingrs)
            else Tree.Node(name, [])
        expa []
    expand "b" |> Tree.visualize (sprintf "%A") |> printfn "%s"
    (*
    a
    └─b
      ├─c
      │ └─d
      ├─d
      └─e *)
    // чтобы сделать a нужно взять d сделать c, после чего взять [c; d; e;] сделать из них b и при помощи b сделать a
    // можно написать так: сделать c, сделать b, сделать a
    let assemble resources lst = 
        let f state x =
            let ingrs = crafts.[x]
            let removes x y = List.fold(fun state x -> List.remove x state) x y
            assert(removes [1; -3; 3; 2; 1] [1;-3;3] = [2; 1])
            let xs = removes state ingrs
            x::xs
        List.fold (f) resources lst
    assert
        let t = expand "b"
        let makes = Tree.notleafs t |> List.rev
        let leafs = Tree.leafs t
        assemble leafs makes = ["b"]
module Case2 = 
    let crafts = 
       [("a", ["b", 2; "c", 3]); 
        ("b", ["c", 4; "d", 6]);
        ("c", ["e", 3; "f", 1]);
        ("d", []);
        ("e", []);
        ("f", ["a", 3]);]
        |> Map.ofList
    let expand = 
        let rec expa last ((name, count) as curr) = 
            let valid xs ys = Set.intersect (set xs) (set ys) |> Set.isEmpty
            assert valid ['a'; 'b'] ['c'; 'd'; 'a']

            let ingrs = crafts.[name] |> List.map (function n, c -> n, c*count)
            if valid last (List.map fst ingrs) then
                 Tree.Node(curr, List.map (expa (name::last)) ingrs)
            else Tree.Node(curr, [])
        expa []
    expand ("a", 1) |> Tree.visualize (sprintf "%A") |> printfn "%s"
    
    let assemble resources lst = 
        let f state (name, count) =
            let f state (name, count) = 
                match Map.tryFind name state with
                | None -> failwithf "%A not found in %A" name state
                | Some count' ->
                    let diff = count' - count
                    if diff < 0 then failwithf "%A - %A = %d" (name, count') count diff
                    elif diff = 0 then Map.remove name state
                    else Map.add name diff state
            assert
                let m = Map.add "a" 10 (Map.empty)
                f m ("a", 2) = Map[("a", 8)]
            let removes x y = List.fold f x y
            assert
                let state = [("d", 24); ("e", 66); ("f", 22)] |> Map.ofList
                let ingrs = [("e", 18); ("f", 6)] //crafts.[name] |> List.map (function n, c -> n, c*count)
                removes state ingrs = Map [("d", 24); ("e", 48); ("f", 16)]
            
            let ingrs = crafts.[name] |> List.map (function n, c -> n, c*count)
            let xs = removes state ingrs
            let v = match Map.tryFind name xs with None -> 0 | Some n -> n
            Map.add name (count + v) xs
        List.fold (f) resources lst
        //f resources lst
    assert
        let req = ("a", 4)
        let t = expand req
        t |> Tree.visualize (sprintf "%A") |> printfn "%s"
        let makes = Tree.notleafs t |> List.rev
        let resources = 
            let leafs = Tree.leafs t
            Seq.groupBy fst leafs
            |> Seq.map (fun (key, v) -> (key, Seq.sumBy snd v))
            |> Map.ofSeq
        assemble resources makes = Map [req]

module case3 =
    type Craft = string * (string * int) list

    let crafts: Craft list = 
       [("a", ["b", 2; "c", 3]); 
        ("b", ["c", 4; "d", 6]);
        ("c", ["e", 3; "f", 1]);]
    let getIgrs = 
        let m = crafts |> Map.ofList
        (fun key -> Map.tryFind key m)
    let diff (resources, res) = 
        let rec f (resources, rest) = function
                | [] -> resources, rest
                | (needName, needCount)::t ->                    
                    match List.partition (fst >> ((=)needName)) resources with
                        | ([(_, avail)], rest) as xs ->
                            if needCount > avail then Some(needName, needCount - avail), rest
                            elif avail = needCount then None, rest
                            else None, (needName, avail-needCount) :: rest
                        | (_, rest) -> Some (needName, needCount), rest
                    |> function
                        | None, res -> f (res, rest) t
                        | Some x, res -> f (res, x::rest) t
        f (resources, res)
    assert
        assert
            diff (["a", 10; "b", 20; "c", 10], []) ["a", 5; "b", 1; "c", 20; "f", 1]
            |> ((=) ([("b", 19); ("a", 5)], [("f", 1); ("c", 10)]))
        assert
            diff (["a", 10; "b", 20; "c", 1], []) ["b", 1; "c", 20; "f", 1] = ([("b", 19); ("a", 10)], [("f", 1); ("c", 19);])
        diff (["a", 1; "b", 20; "c", 10], []) ["e", 1; "a", 2; "c", 2;] = ([("c", 8); ("b", 20)], [("a", 1); ("e", 1)])

    let makeSingle resources x =
        match diff (resources, []) [x] with
            | resources, [] -> resources, []
            | resources, ([needName, needCount] as rest) ->
                match getIgrs needName with
                | Some ingrs -> 
                    let parts = ingrs |> List.map (fun (name,n) -> name, n*needCount)
                    diff (resources, []) parts
                | None -> resources, rest
    assert
        assert
            makeSingle ["e", 1; "a", 2; "c", 2] ("a", 1) = ([("a", 1); ("e", 1); ("c", 2)], [])
        assert
            makeSingle ["e", 1; "a", 2; "c", 2] ("d", 1) = ([("e", 1); ("a", 2); ("c", 2)], [("d", 1)])
        assert
            makeSingle ["e", 1; "a", 2; "c", 2] ("e", 2) = ([("a", 2); ("c", 2)], [("e", 1)])
        assert
            makeSingle [] ("a", 1) = ([], [("c", 3); ("b", 2)]);
        assert
            makeSingle ["a", 1] ("a", 2) = ([], [("c", 3); ("b", 2)]);
        assert
            makeSingle ["e", 1; "a", 2; "c", 2] ("a", 3) = ([("e", 1)], [("c", 1); ("b", 2)])
        true

    let rec make (resources, rest) lst = 
        match diff (resources, rest) lst with
        | resources, [] -> resources, []
        | resources, rest ->
            let makeSingle resources (needName, needCount) =
                match getIgrs needName with
                | Some ingrs -> 
                    let parts = ingrs |> List.map (fun (name,n) -> name, n*needCount)
                    Some <| diff (resources, []) parts
                | None -> None
            failwith ""
    //make ["e", 1; "a", 2; "c", 2] (["a", 1; "b", 20; "c", 10], []) //= ([[("e", 1)]], [("c", 2); ("b", 16)])