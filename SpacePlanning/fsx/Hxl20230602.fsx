module Locations =

    // Adjacent Locations
    type Loc = 
        { 
            x : int
            y : int
            z : int
        }
    
    // Identity Location
    let idnLoc = 
        {
            x = 0
            y = 0
            z = 0
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
        (loc: Loc * int) 
        (occ: Loc[]) = 
        match loc with 
        | x,y when y >= 0 -> 
            let inc = x 
                    |> adjLocs 
                    |> Array.tail 
                    |> Array.except occ 
                    |> Array.tryHead
            match inc with 
            | Some a -> a, y
            | None -> loc
        | _ -> loc
   
    // Increment Locations
    let incLocs 
        (loc: (Loc*int)[]) 
        (occ: Loc[]) = 
        Array.scan (fun ac st -> 
        incLoc st (Array.append[|fst ac|] occ )) 
            loc[0] loc
            |> Array.tail

    // Get Locations
    let getLoc 
        (loc : (Loc*int)[]) = 
        loc
        |> Array.map(fun x 
                        -> fst x)

    // Available Location
    let avlLoc 
        (loc: (Loc*int))
        (occ: Loc[]) = 
        loc 
        |> fst 
        |> adjLocs 
        |> Array.except occ 
        |> Array.length

    // Cluster Locations
    let clsLocs 
        (loc: (Loc*int)[])
        (occ : Loc[]) = 
        let cnt = 
                loc
                |> Array.map (fun x -> snd x)
                |> Array.max
        let acc = Array.chunkBySize 1 loc
        
        let rec clsts 
            (loc: (Loc*int)[])
            (occ : Loc[])
            (acc:(Loc*int)[][])
            (cnt : int) = 
            match cnt with 
            | c when c < 1 -> acc
            | _ -> 
                    let occ = 
                        acc 
                        |> Array.concat 
                        |> getLoc
                        |> Array.append occ
                        |> Array.distinct
                    let rpt = Array.map (fun x 
                                            -> (snd x)-1) loc
                    let loc =  
                        acc
                        |> Array.map (fun x
                                        -> Array.filter (fun a 
                                                            -> (avlLoc a occ) > 0) x)
                        |> Array.map (fun x 
                                        -> Array.tryHead x)
                        |> Array.map (fun x 
                                        -> match x with
                                           | Some a -> a 
                                           | None -> (idnLoc,-1))                
                        |> Array.map2 (fun x y 
                                        -> fst y,  x) rpt
                    let inc = incLocs loc occ
                            
                    let acc = Array.map2  (fun x y
                                            -> Array.append x y) 
                                acc
                                (Array.chunkBySize 1 inc)
                    (clsts loc occ acc (cnt-1))
                
        clsts loc occ acc cnt
        |> Array.map(fun x 
                        -> Array.filter(fun (y,z) -> z >= 0) x)
        
        


    
// Test Zone
//Hexels.adjHxls {Avl=6; Cls = 1; Loc=(1,-2,0)} [||]
let og:Locations.Loc = {x=0; y=0; z=0}
let t0 = Locations.adjLocs og
let t1 = Array.zip t0 [|8;2;8;8;8;8;8|]
//let t2 = Locations.incLoc t1[1] t0
let t2 = Locations.clsLocs t1[1..6] t0

//let t11 = Array.concat[|t1;Array.concat t2|]
//let o11 = Array.map (fun x -> fst x) t11
//let t12 = Locations.clsLocs t2 o11

