module Program

open System.Windows.Forms
open System

#if INTERACTIVE
#load "tree.fs"
#load "Expander.fs"
#endif

type ReciplesType = (string * (int * (string * int) list)) list
(*
module realSample =
    /// <summary>
    /// Заменяет название предметов на [a, b, c, ...]
    /// </summary>
    /// <param name="reciples"></param>
    let simples reciples =
        let reciples = Map.toList reciples
        let rec f acc i = function
            | [] -> List.rev acc
            | h::t -> f ((h, char i |> string)::acc) (i + 1) t
        let dic = List.map fst reciples |> f [] (int 'a') |> Map.ofList
        reciples
        |> List.map (fun (name, (made, xs)) -> dic.[name], (made, List.map (fun (x, need) -> Map.find x dic, need) xs))
        |> Map.ofList
    let reciples =
        ["alchemy engine", (1, ["boards", 4; "cut stone", 2; "electrical doodad", 2]);
         "boards", (1, ["log", 4]);
         "cut stone", (1, ["rocks", 3]);
         "electrical doodad", (1, ["boards", 4; "gold nugged", 2; "cut stone", 1]);
         "log", (1, []);
         "rocks", (1, []);
         "gold nugged", (1, [])] |> Map.ofList
    //let reciples2 = simples reciples
    let startRes = [ "b", 8; ] |> Map.ofList
    let req = "alchemy engine", 1
    Expander.expand reciples req |> Tree.visualize |> printfn "%s"
    let (res, result) = Expander.expand2 reciples startRes req
    Expander.assemble reciples (Map.ofList res) result
    |> ignore
    *)

module serial =
    open System.IO
    open System.Runtime.Serialization.Formatters.Binary

    let load path =
        use fileStream = new FileStream(path, FileMode.Open)
        let bf = new BinaryFormatter()
        bf.Deserialize(fileStream)

    let save path thing =
        let serializeThing thing =
            let bf = new BinaryFormatter()
            use mstream = new MemoryStream()
            bf.Serialize(mstream, thing)
            mstream.ToArray()
        let byteArr = serializeThing thing
        use fileStream = new FileStream(path, FileMode.Create)
        fileStream.Write(byteArr, 0, byteArr.Length)

module form = 
    let form = new RecipleInputForm.Form1()
    // <reciple name = "rec1" make = 1><ingr name = "1" need = 1 />...<name>
    let bdpath = "bd.dat"
    //form.txbName.AutoCompleteCustomSource.Add("aaaa".ToString()) |> printfn "%d"
    
    let mutable reciples = 
        if System.IO.File.Exists bdpath then
            IO.File.ReadAllText bdpath |> fun x -> Newtonsoft.Json.JsonConvert.DeserializeObject<ReciplesType> x
             // serial.load bdpath :?> ReciplesType
        else []
        |> Map.ofList
    let txbIngrs = 
        let indent = 40
        let loc x y (c:Control) = c.Location <- Drawing.Point(x, y)
        let ltxb, lnum = form.txbName.Location, form.numMake.Location
        let txbIngrs = 
            List.init 10 (fun i -> 
                let txb, num = new TextBox(), new NumericUpDown()
                txb.AutoCompleteMode <- AutoCompleteMode.SuggestAppend
                txb.AutoCompleteSource <- AutoCompleteSource.CustomSource
                txb.Size <- form.txbName.Size
                num.Size <- form.numMake.Size
                num.Maximum <- form.numMake.Maximum
                loc ltxb.X (indent*(i+1) + ltxb.Y) txb
                loc lnum.X (indent*(i+1) + lnum.Y) num
                txb, num)
        txbIngrs |> Array.ofList |> Array.collect (fun (txb, num) -> [|txb :> Control; num :> Control|])
        |> form.panel1.Controls.AddRange
        txbIngrs
    do
        let names = reciples |> Map.toArray |> Array.map fst
        form.txbName.AutoCompleteCustomSource.AddRange names
        txbIngrs |> List.iter (fun (txb, _) -> txb.AutoCompleteCustomSource.AddRange names)

    let autoComleteAdd s =
        form.txbName.AutoCompleteCustomSource.Add s |> ignore
        txbIngrs |> List.iter (fun (txb, _) -> txb.AutoCompleteCustomSource.Add s |> ignore)

    let clear () =
        form.txbName.Text <- ""; form.numMake.Text <- "1"
        txbIngrs |> List.iter (fun (txb, num) -> txb.Text <- ""; num.Text <- "1")
    clear()
    do // выделение цифр при выборе количества предметов
        let selectAll (x:NumericUpDown) = x.Select(0, x.Text.Length)
        let init (x:NumericUpDown) = x.Enter.Add (fun _ -> selectAll x)
        
        init form.numMake
        txbIngrs |> List.iter (snd >> init)

        let selectAll (x:TextBox) = x.Select(0, x.Text.Length)
        let init (x:TextBox) = 
            x.Enter.Add (fun _ -> selectAll x)
        init form.txbName
        txbIngrs |> List.iter (fst >> init)



    form.btnInput.Click.Add (fun _ ->
        if form.txbName.Text = "" then MessageBox.Show "input name item" |> ignore
        else
            let ingrs = 
                txbIngrs |> List.filter (fun (txb, _) -> txb.Text <> "")
                |> List.map(fun (txb, num) -> txb.Text, int num.Text)
            //let res = form.txbName.Text, (int form.numMake.Text, ingrs)
            form.lstUnknown.Items.Remove form.txbName.Text
            ingrs
            |> List.iter (fun (x, _) -> 
                if not <| Map.containsKey x reciples then
                    form.lstUnknown.Items.Add x |> ignore)
            reciples <- Map.add form.txbName.Text (int form.numMake.Text, ingrs) reciples
            autoComleteAdd form.txbName.Text
            clear ())
    form.lstUnknown.SelectedIndexChanged.Add(fun _ ->
        if form.lstUnknown.SelectedIndex <> -1 then
            let str = form.lstUnknown.Items.Item form.lstUnknown.SelectedIndex :?> string
            form.txbName.Text <- str)
    form.button1.Click.Add(fun _ ->
        Console.Clear()
        let startRes = 
            txbIngrs |> List.filter (fun (txb, _) -> txb.Text <> "")
            |> List.map(fun (txb, num) -> txb.Text, int num.Text)
            |> Map.ofList
        let req = form.txbName.Text, int form.numMake.Text
        Expander.expand2Start reciples startRes req |> printfn "%A"
        )
    form.FormClosing.Add(fun _ ->
        let reciples = Map.toList reciples
        //serial.save bdpath reciples
        Newtonsoft.Json.JsonConvert.SerializeObject(reciples, Newtonsoft.Json.Formatting.Indented) |> fun s -> IO.File.WriteAllText(bdpath, s)
        
        System.IO.File.WriteAllLines("reciplesRaw.txt", List.map (sprintf "%A") reciples))

[<STAThread; EntryPoint>]
let main _ =
    Application.Run(form.form)
    0