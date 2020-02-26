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
#if INTERACTIVE
#load "Expander.fs"
#endif
open Expander

let expandTest () =
    let reciples : Reciples =
        Json.desf @"Info\OldDBs\starbound.json"
    let stocks : Map<ItemName, HaveInStock> = Map.ofList []
    let req = ("крафт", 1)

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
    f exp = f act

module realSample =
    /// Заменяет название предметов на [a, b, c, ...]
    let simples reciples =
        let reciples = Map.toList reciples
        let rec f acc i = function
            | h::t -> f ((h, char i |> string)::acc) (i + 1) t
            | [] -> List.rev acc
        let dic = List.map fst reciples |> f [] (int 'a') |> Map.ofList
        reciples
        |> List.map (fun (name, (made, xs)) -> dic.[name], (made, List.map (fun (x, need) -> Map.find x dic, need) xs))
        |> Map.ofList
    let reciples : Expander.Reciples =
        [
            "alchemy engine", (1, ["boards", 4; "cut stone", 2; "electrical doodad", 2])
            "boards", (1, ["log", 4])
            "cut stone", (1, ["rocks", 3])
            "electrical doodad", (1, ["boards", 4; "gold nugged", 2; "cut stone", 1])
            "log", (1, []);
            "rocks", (1, []);
            "gold nugged", (1, [])
        ] |> Map.ofList
    //let reciples2 = simples reciples
    let startRes = [ "b", 8 ] |> Map.ofList
    let req = "alchemy engine", 1
    simples reciples
    Expander.expand reciples req |> Tree.visualize |> printfn "%s"
    let (res, result) = Expander.expand2 reciples startRes req
    Expander.assemble reciples (Map.ofList res) result
    |> ignore

// module reciplesMod =
//     let state1 =
//         [ ("a", (1, [("f", 1); ("b", 1)]));
//             ("b", (1, [("f", 1)]));
//             ("f", (1, [("e", 1);]));
//             ("e", (100, [("g", 1)]));
//             "g", (1, [])
//             ] |> Map.ofList
//     let state2 =
//         [("iron casing", (2, [("iron plate", 1); ("forge hammer", 1)]));
//         ("iron plate", (1, [("iron", 1); ("forge hammer", 1)]));
//         ("forge hammer", (1, []));
//         ("iron", (1, []));
//         ("craft", (1, [("iron casing", 2); ("iron plate", 1)]));
//         ] |> Map.ofList
// let reciples = reciplesMod.state2
// let req = ("craft", 1)
// let res = Map["iron plate", 1]
// let x = Expander.expandNotMod reciples req
// Tree.visualize (sprintf "%A") x |> printfn "%s"
// let (r, t) = Expander.expand2.expand res x
// Tree.visualize (sprintf "%A") t |> printfn "%s"


// let res', makes' =
//     let raw = Expander.expand2.makes t
//     List.head raw, List.tail raw |> List.concat

// let resources = List.fold Expander.expand2.give res res'
// Expander.assemble reciples resources makes' |> printfn "%A"

// module test1 =
//     let raw = Node (("a", 1, 1), [Node(("b", 1, 1), [])])
//     let r, tree = Expander.expand2.expand (Map["a", 1]) raw
//     tree |> Tree.visualize (sprintf "%A") |> printfn "%s"
//     let raw' =
//         Node (("craft", 1, 1),
//             [Node (("iron casing", 2, 2),
//                 [Node (("iron plate", 1, 1),
//                     [Node (("iron", 1, 1),[]);]);]);
//             Node (("iron plate", 1, 1), [Node (("iron", 1, 1),[]);])])
//     let r', tree' = Expander.expand2.expand (Map["iron plate", 1]) raw'
//     tree' |> Tree.visualize (sprintf "%A") |> printfn "%s"
(*
res = ["e", 0]
val t : Noderee<string * int> =
  T (("a", 1),
     [T (("f", 1),[T (("e", 1),[T (("g", 1),[])])]);
      T (("f", 1),[T (("e", 0),[T (("g", 0),[])])])])
val res' : Map<string,int> = map [("a", 0); ("e", 98); ("f", 0)]
res = ["e", 1]
val t : Noderee<string * int> =
  T (("a", 1),
     [T (("f", 1),[T (("e", 0),[T (("g", 0),[])])]);
      T (("f", 1),[T (("e", 1),[T (("g", 1),[])])])])
val res' : Map<string,int> = map [("a", 0); ("e", 99); ("f", 0)]
*)

(*
e, 0
("a", 1)
├─("f", 1)
│ └─("e", 1)
│   └─("g", 1)
└─("b", 1)
  └─("f", 1)
    └─("e", 0)
      └─("g", 0)

e, 1
("a", 1)
├─("f", 1)
│ └─("e", 0)
│   └─("g", 0)
└─("b", 1)
  └─("f", 1)
    └─("e", 1)
      └─("g", 1) *)