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
        (loc: Loc * int) 
        (occ: Loc[]) = 
        match loc with 
        | x,y when y > 0 -> 
            let inc = x 
                    |> adjLocs 
                    |> Array.tail 
                    |> Array.except occ 
                    |> Array.tryHead
            match inc with 
            | Some a -> a, y-1
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

    // Available Location
    let avlLoc 
        (occ: Loc[])
        (loc: (Loc*int))  = 
        loc 
        |> fst 
        |> adjLocs 
        |> Array.except occ 
        |> Array.length > 0


    // Cluster Locations
    let clsLocs 
        (loc: (Loc*int)[])
        (occ : Loc[]) = 
        
        let rec clsts 
            (loc: (Loc*int)[])
            (occ : Loc[]) = 
            let acc = Array.chunkBySize 1 loc
            let cnt = 
                loc
                |> Array.map (fun x -> snd x)
                |> Array.max

            match cnt with 
            | c when c < 1 -> acc
            | _ -> 
                let cnt = cnt - 1
                let inc = incLocs loc occ
                let occ = Array.append  occ (Array.map(fun x -> fst x)inc) 
                //Filter Available
                let avl = 
                    acc 
                    |> Array.map (fun x-> Array.filter (avlLoc occ)x)
                let loc = Array.map2 (fun x y 
                                        -> Array.append x y)avl [|loc|]
                                        |> Array.map (fun x -> Array.head x)
                
                let acc = Array.map2 
                            (fun x y 
                                -> Array.append x y) 
                            acc
                            (clsts loc occ)
                acc
        clsts loc occ 
        


    
// Test Zone
//Hexels.adjHxls {Avl=6; Cls = 1; Loc=(1,-2,0)} [||]
let og:Locations.Loc = {x=0; y=0; z=0}
let t0 = Locations.adjLocs og
let t1 = Array.zip t0 [|1;3;8;5;1;1;1|]
//let t2 = Locations.incLoc t1[1] t0
let t2 = Locations.clsLocs t1[1..3] t0
//let t3 = Locations.clsLocs t1[1..3] t0

//Locations.addLocs t1[1..2] t1 2