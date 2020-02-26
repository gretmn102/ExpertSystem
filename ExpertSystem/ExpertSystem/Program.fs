module Program

open System.Windows.Forms
open System

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
type Reciples = Expander.Reciples
open FsharpMyExtension
module form = 
    let form = new RecipleInputForm.Form1()
    let bdpath = "bd.json"
    let mutable reciples : Reciples =
        if System.IO.File.Exists bdpath then
            Json.desf bdpath
        else Map.empty
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
        let names =
            reciples |> Map.toArray |> Array.map fst
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
            let reciples' = reciples
            ingrs
            |> List.iter (fun (x, _) ->
                if not <| Map.containsKey x reciples' then
                    form.lstUnknown.Items.Add x |> ignore)
            
            let reciple =
                {
                    Expander.Ingredients = ingrs
                    Expander.ItemName = form.txbName.Text
                    Expander.OutputCount = int form.numMake.Text
                }
            reciples <-
                Map.add form.txbName.Text reciple reciples'
                // |> Expander.Reciples

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
        Json.serf bdpath reciples

        Seq.map (fun (KeyValue(k, v)) -> sprintf "%A" (k, v)) reciples
        |> uncurry System.IO.File.WriteAllLines "reciplesRaw.txt"
    )

[<STAThread; EntryPoint>]
let main _ =
    Application.Run(form.form)
    0