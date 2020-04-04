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
    let mci (ar : (int)list) (hc : (float*float)list) (oc : (float*float)list) =
        // ct : Cluster count
        // hc : Host cluster
        // oc : Occupied cells
        // ic : Increment
        let ic = 2
        let ct = List.length ar
        let aa hc oc=
            // Required number of base cells
            List.filter (fun x -> 
                List.contains x (List.append hc oc 
                |> prm)) (prm hc) 
            |> List.truncate  ct
            // Mini clusters of specified count around base cells
            |> List.map (fun x -> cls ic oc x)

        let bb hc oc = 
            List.scan (fun x -> List.append x) [] (aa hc oc )
            |> List.truncate ct

        let cc hc oc = List.map2 (fun x y -> List.except x y) (bb hc oc) (aa hc oc)

        cc hc oc

    let mcs (ct : int) (hc : (float*float)list) =  
        //Cluster of specified count and host cluster
        let clh ic hc oc =
            (hc @ oc)
            |> prm
            |> List.head
            |> cls ic (hc @ oc)
            |> List.append (hc @ oc)
        //a 10 hc []   
        let st = 
            let ic = 3
            List.replicate ct (List.truncate 1 (prm hc))
            |> List.scan (fun x y -> clh ic x y) (List.truncate 1 (prm hc))
            |> List.map (fun x -> List.distinct x)
        let rr = List.map2 (fun x y -> List.except x y) (List.truncate ct st) (List.tail st)  
        rr
    

    // Tuples to coordinates
    let tupXY tup  =
        let hexCl = tup |> List.unzip
        [ fst(hexCl) ; snd(hexCl) ]

    let tupLst tup = List.map (fun x -> tupXY x) tup

    let fd = (cls 7 [] (0.0,0.0) )
    let cc = mcs 3 fd
    //let ca = mcs cc cc

    
    //let f = List.replicate 3 ([(0.0,0.0)])|> List.scan (fun x -> (mcs x))([(0.0,0.0)]) |> List.tail
    // |> List.scan (fun x -> mcs)
    //let aa = mci [5;6;7] (cls 7 (0.0,0.0) []) (cls 15 (1.86,1.074)[])
    //let zz = cls 40 ( 0.0,0.0) []
    //let ll = zz.Length
    //let cc = prm zz 
    //let lg = cc.Length