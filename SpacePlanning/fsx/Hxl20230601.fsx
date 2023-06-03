module Locations =
    //Adjacent Locations
    type Loc = 
        { 
            x : int
            y : int
            z : int
        }
    let adjLocs 
        (loc: Loc) = 
        Array.map3 (fun a b c ->
            {
                x = a + loc.x 
                y = b + loc.y
                z = c + loc.z
            })
            [|0; 1; -1; -2; -1; 1; 2|]
            [|0; -2; -2; 0; 2; 2; 0|]
            [|0; 0; 0; 0; 0; 0; 0|]

    // Increment Location
    let incLoc 
        (loc: Loc) 
        (occ: Loc[]) = 
        let inc = loc 
                |> adjLocs 
                |> Array.tail 
                |> Array.except occ 
                |> Array.tryHead
        match inc with 
        | Some a -> a
        | None -> loc
   
    // Increment Locations
    let incLocs 
        (loc: Loc[]) 
        (occ: Loc[]) 
        (cnt: int[]) = 

        let inc = 
            Array.scan (fun ac st -> 
            incLoc st ([|[|ac|];occ|] 
            |> Array.concat)) loc[0] loc
                |> Array.tail
        
        Array.map3 (fun l i c -> 
            match c with
            | a when a < 1 -> l
            | _ -> i) loc inc cnt

    // Additional Locations
    //let addLocs (loc: (int * int * int)[]) (occ: (int * int * int)[]) (cnt : int[]) = 
  
    // Available Location
    let avlLoc 
        (loc: Loc) 
        (occ: Loc[]) = 
        loc |> adjLocs |> Array.except occ |> Array.length

    // Available Locations
    let avlLocs 
        (loc: Loc[]) 
        (occ: Loc[]) = 
        let a1 = incLocs loc occ [||]
        Array.map(fun x -> avlLoc x (Array.concat[|a1;loc;occ;|])) a1
        
module Hexels = 
    // Hexel
    type Hxl = 
        { 
            Avl : int
            Cls : int 
            Loc : Locations.Loc
        }

    // Hexel Locations
    let locOfHxl 
        (hxl : Hxl[]) = 
        Array.map (fun x -> x.Loc) hxl

    // Locations to Hexels
    let locToHxl 
        (avl : int[]) 
        (cnt : int[]) 
        (loc : Locations.Loc[]) = 
        Array.map2 (fun a b  -> 
        {Avl = a ; Cls =1;  Loc = b}) avl loc

    // Adjacent Hexels
    let adjHxls 
        (hxl : Hxl) 
        (occ : Hxl[]) = 
        match hxl.Avl with 
        | a when a < 1 -> hxl |> Array.singleton
        | _ -> 
                let adjLocs = (Locations.adjLocs hxl.Loc)
                let occ01 = Array.concat ([|adjLocs;(locOfHxl occ)|])
                let avlLocs = (Locations.avlLocs adjLocs occ01)
                Array.map2 (fun c d -> 
                    {Avl = c; Cls = 1; Loc = d}) avlLocs adjLocs

// Test Zone
//Hexels.adjHxls {Avl=6; Cls = 1; Loc=(1,-2,0)} [||]
//let og:Locations.Loc = {x=0; y=0; z=0}
//let t1 = Locations.adjLocs og
//let t2 = Locations.incLocs t1[1..6] t1
//let t3 = Locations.avlLocs t1[1..6] t1

//Locations.addLocs t1[1..2] t1 2