namespace Clusters

module Tup =
 //Convert for export
// Tuples to coordinates
   let tupXY tup  =
       let hexCl = tup |> List.unzip
       [ fst(hexCl) ; snd(hexCl) ]

   let tupLst tup = List.map (fun x -> tupXY x) tup

module Hexel =
    // Core and six adjacent cells
    let adj (x : int, y: int) =
        // x : x coordinate
        // y : y coordinate
        let hxx = [ 0; -1; -2; -1; 1; 2; 1 ] |> List.map (fun i -> x + i)
        let hxy = [ 0; -2; 0; 2; 2; 0; -2 ] |> List.map (fun i -> y + i)
        (hxx,hxy) ||> List.zip

    // Cell cluster by specified count
    let cls (ct : int) (oc : (int * int)list) (x : int , y : int)  =
        // ct : Number of cells required
        // oc : List of occupied cells (can be none)

        let ls = adj (x,y)
                |> List.distinct
                |> List.except oc

        let rec cl ar ls oc =

            match ls with
            |[] -> []
            |x :: y when ls |> List.length < ct ->
                cl ar (y @ adj (x)
                    |> List.distinct
                    |> List.except oc) oc
            |_ -> 
                List.take ar ls
                |> List.distinct
                |> List.except oc

        cl ct ls oc

    // Perimeter cells
    let prm (ce : (int*int)list) =
        // ce : All cells in cluster

        List.filter (fun x -> 
            adj x |> List.map (fun x -> 
                List.contains x ce) 
                |> List.contains false) ce

    // Concentric cells
    let con hc oc = 
        List.except oc (List.map (fun x -> adj x) hc 
        |> List.concat 
        |> List.distinct) 
        |> prm

    // Start Cells of Sub Cluster
    let rec scl (ct : int) (oc : (int * int)list) (hc : (int * int)list) = 
        // ct : Cluster Count
        // oc : Occupied Cells
        // hc : Host Cells
        match (List.length (con hc oc) > ct) with
        //Check if perimeter has adequate host cells
        | true -> [List.truncate ct ((con hc oc)); hc]
        //Increase host cluster count to satisfy First Cell Count
        //For corridors shift hc @ to end in the line below
        | false -> scl ct (hc @ (List.truncate 1 (con hc oc))) oc
    
    let clc (ar :int list) (oc : (int * int)list) (hc : (int * int)list) =
        let sc1 = scl (List.length ar) oc hc
        let sc2 = List.map (fun x -> scl 1 (oc @ List.tail hc) (x :: []) |> List.head) sc1.[0]
        
        sc2

    let gv = scl 3 [] [(0,0)]
    let gb = clc [10;15;5] [] [(0,0)]