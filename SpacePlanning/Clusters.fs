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
            adj x |> List.map (fun x -> 
                List.contains x ce) 
                |> List.contains false) ce
    
    // Incremental cells at multiple clusters
    let mcl (ct : int) (hc : (float*float)list) (oc : (float*float)list) =
        // ct : Cluster count
        // hc : Host cluster
        // oc : Occupied cells
        // ic : Increment
        let ic = 3
        let a =
            // Required number of base cells
            List.filter (fun x -> 
                List.contains x (List.append hc oc 
                |> prm)) (prm hc) 
            |> List.truncate  ct
            // Mini clusters of specified count around base cells
            |> List.map (fun x -> cls x ic oc)

        let b = 
            List.scan (fun x -> List.append x) [] a 
            |> List.truncate ct

        List.map2 (fun x y -> List.except x y) b a

    // Tuples to coordinates
    let tupXY tup  =
        let hexCl = tup |> List.unzip
        [ fst(hexCl) ; snd(hexCl) ]

    let tupLst tup = List.map (fun x -> tupXY x) tup

    let aa = List.map (fun x -> tupXY x) (mcl 3 (cls(0.0,0.0) 15 []) [])
    //let zz = cls ( 0.0,0.0) 40 []
    //let ll = zz.Length
    //let cc = prm zz 
    //let lg = cc.Length