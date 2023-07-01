let spaceStr = "(1/5/Foyer),(2/20/Living),(3/20/Dining),
    (4/20/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
    (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
    (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
    (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"

let spcId11 = 
    ((spaceStr.Replace ("\n",""))
        .Replace(" ",""))
        .Split ","
        |> Array.map(fun x -> x.Remove(0,1)) 
        |> Array.map(fun x -> x.Remove(x.Length-1,1))
        |> Array.map (fun x -> x.Split "/") 
        |> Array.map (fun x -> (x[0],int x[1],x[2])) 
        |> Array.sortBy (fun (x,y,z) -> x)
        |> Array.groupBy(fun (x,_,_) -> match (x.Length <= 1) with 
                                        |true -> "0"
                                        |false -> x.Substring (0, x.LastIndexOf(".")))
let spcId01 = 
    ((spaceStr.Replace ("\n",""))
        .Replace(" ",""))
        .Split ","
        |> Array.map(fun x -> x.Remove(0,1)) 
        |> Array.map(fun x -> x.Remove(x.Length-1,1))
        |> Array.map (fun x -> x.Split "/") 
        |> Array.map (fun x -> (x[0],int x[1],x[2])) 
        |> Array.sortBy (fun (x,y,z) -> x)
        |> Array.groupBy(fun (x,_,_) -> match (x.Length <= 1) with 
                                        |true -> x
                                        |false -> x.Substring (0, x.LastIndexOf(".")))
        |> Array.unzip

let axsId02 = 
    let getIndex elm arr =  Array.findIndex(fun x -> x = elm) arr
    let getIndices elm arr = Array.map (fun x -> getIndex x arr) elm
    let axsId01 = spcId01 |> fst
                |> Array.filter (fun y -> Array.length (y.Split ".") = 1)

    getIndices 
        axsId01 
        (fst spcId01)
    |> Array.windowed 2
    |> Array.map (fun x -> x[0],x[1])

let spcId02 = 
    axsId02 
    |> Array.map (fun (x,y) 
                    -> x, Array.concat 
                        [|(snd spcId01)[x];
                        [|Array.head ((snd spcId01)[y])|]|])
    |> Array.unzip


let idSzLb =
    (snd spcId01)
    |> Array.mapi (fun index item ->
        match Array.tryFindIndex ((=) index) (fst spcId02) with
        | Some i -> (snd spcId02).[i]
        | None -> item
    )
    |> Array.removeAt (Array.length (fst spcId01) - 1)