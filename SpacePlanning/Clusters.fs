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
    let cls (ct : int) oc (x : double , y : double)  =
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

    // Incremental cells at multiple clusters (start)
    let mcs (ct : int) (hc : (float*float)list) =  
        // hc : Host cluster
        // ct : Number of sub clusters

        //Cluster of specified count and host cluster
        let clh ic hc oc =
            (hc @ oc)
            |> prm
            |> List.head
            |> cls ic (hc @ oc)
            |> List.append (hc @ oc)
        //Multiple clusters with common host 
        let mlc = 
            let ic = 3
            List.replicate ct (prm hc)
            |> List.scan (fun x y -> clh ic x y) (prm hc)
            |> List.map (fun x -> List.distinct x)
        List.map2 (fun x y -> List.except x y) (List.truncate ct mlc) (List.tail mlc)

    // Tuples to coordinates
    let tupXY tup  =
        let hexCl = tup |> List.unzip
        [ fst(hexCl) ; snd(hexCl) ]

    let tupLst tup = List.map (fun x -> tupXY x) tup

    let fd = (cls 1 [] (0.0,0.0) )
    let cc = mcs 10 fd
