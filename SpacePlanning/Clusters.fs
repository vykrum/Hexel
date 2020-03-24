namespace Clusters
module HexShapes =
    open System

    // Core and six adjacent cells
    let adj (x : float, y: float) =
        // x : x coordinate
        // y : y coordinate

        let hxx = [ 0.0; 0.0; 0.93; 0.93; 0.0; -0.93; -0.93 ]
                |> List.map (fun i -> Math.Round(x + i,3))

        let hxy = [ 0.0; 1.074; 0.537; -0.537; -1.074; -0.537; 0.537 ]
                |> List.map (fun i -> Math.Round(y + i, 3))

        (hxx,hxy) ||> List.zip
    
    // Cell cluster by specified count
    let cls (x : double , y : double) (ct : int) oc =
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

    // Perimeter cells in cluster
    let prm (ce : (float*float)list) =
        // ce : All cells in cluster

        List.filter (fun x -> 
            adj x 
            |> List.map (fun x -> 
                List.contains x ce) 
                |> List.contains false) ce

    let mlt (ct : (int)list) (ce : (float*float)list) (oc : (float*float)list) =
        // ct : List of cell counts
        // ce : List of initial cells
        // oc : List of occupied cells
        // ic : Increment
        let ic = 3
        List.filter (fun x -> 
            List.contains x (List.append ce oc 
            |> prm)) (prm ce) 
        |> List.truncate (List.length ct) 
        |> List.map (fun x -> cls x ic oc) 
        |> List.scan (fun x -> List.except x) []
        |> List.tail 
      

    // Tuples to coordinates
    let tupXY tup  =
        let hexCl = tup |> List.unzip
        [ fst(hexCl) ; snd(hexCl) ]

    let aa = mlt [10;20;30] (adj(0.0,0.0)) []
    //let zz = cls ( 0.0,0.0) 40 []
    //let ll = zz.Length
    //let cc = prm zz 
    //let lg = cc.Length