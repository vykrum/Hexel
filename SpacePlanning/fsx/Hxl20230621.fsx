let spaceStr = "(1/5/Foyer),(2/20/Living),(3/20/Dining),
    (4/20/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
    (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
    (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
    (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"

let spaceMap = 
    ((spaceStr.Replace ("\n",""))
        .Replace(" ",""))
        .Split ","
        |> Array.map(fun x -> x.Remove(0,1)) 
        |> Array.map(fun x -> x.Remove(x.Length-1,1))
        |> Array.map (fun x -> x.Split "/") 
        |> Array.map (fun x -> (x[0],(int x[1],x[2]))) 
        |> Array.sortBy (fun (x,y) -> x)
        |> Map.ofArray

let spcKy01 = 
    spaceMap 
    |> Map.keys 
    |> Array.ofSeq 
    |> Array.groupBy(fun x -> match (x.Length <= 1) with 
                                        |true -> "0"
                                        |false -> x.Substring (0, x.LastIndexOf(".")))
