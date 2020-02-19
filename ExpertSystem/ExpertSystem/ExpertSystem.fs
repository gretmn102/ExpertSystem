module ExpertSystem

#if INTERACTIVE
#load "expander.fs"
#endif
type ReciplesType = (string * (int * (string * int) list)) list

// что делать с "forge hammer"?
// Суть: для получения "iron plate" использует "forge hammer" и "iron ingot".
// После крафта - возвращается "forge hammer" на одну единицу прочности. Всего у него - 80 единиц.
// Можно сделать его рецепт так:
// { Name = "forge hummer"; Prod = 80; Ingredients = [...] }
// Но есть одна преграда: "industrial workbench" для создания требует целый "forge hammer"
// Можно написать в рецепте:
// {
//     Name = "industrial workbench"; Prod = 1
//     Ingredients = [
//         ...
//         "forge hummer", 80
//         ...
//     ]
// }
// Но что делать, когда пользователь напишет, что у него на складе два "forge hammer" по 40 прочности?
// Для рецепта не годится. Такая вот досада.

// Про устройства с электропотреблением и модификациями -- и говорить нечего: та еще морока.

let temperyFunction () =
    let reciples =
        let reciples : ReciplesType =
            Json.desf @"E:\Project\ExpertSystem\ExpertSystem\ExpertSystem\bin\Debug\net45\bd.dat"
        Map.ofList reciples
    // Map.find "industrial workbench" reciples
    let stocks = 
        [
            "forge hummer", 999
            "cutter", 999
            "blast furnace", 999
            "compressor", 999
            "thermal centrifuge", 999
            "mining laser", 1
            // "glass pane", 12
            // "iron item casing", 1
            // "iron ingot", 9
            "tin cable", 4
            "heat conductor", 1
        ]
    // let req = "thermal centrifuge", 1
    let req = "nuclear reactor", 1
    // let req = "blast furnace", 1
    // let req = "blast furnace", 1
    Expander.expandWithTest reciples (Map.ofList stocks) req
    |> List.filter (List.isEmpty >> not)
    // |> List.iter (printfn "%A")

    Map.find "crushed copper ore" reciples