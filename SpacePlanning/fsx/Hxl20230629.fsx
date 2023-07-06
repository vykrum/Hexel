type Loc = 
    { 
        x : int
        y : int
        z : int
    }

// Identity Location
let identity = 
    {
        x = 0
        y = 0
        z = 0
    }

// Adjacent Location
let adjacent 
    (ccw: bool)
    (loc: Loc) = 
    match ccw with 
    | true -> Array.map3 (fun a b c ->
                {
                    x = a + loc.x 
                    y = b + loc.y
                    z = c + loc.z
                })
                [|0; 1; 2; 1; -1; -2; -1|]
                [|0; -2; 0; 2; 2; 0; -2|]
                [|0; 0; 0; 0; 0; 0; 0|]
    | false -> Array.map3 (fun a b c ->
                {
                    x = a + loc.x 
                    y = b + loc.y
                    z = c + loc.z
                })
                [|0; 1; -1; -2; -1; 1; 2|]
                [|0; -2; -2; 0; 2; 2; 0|]
                [|0; 0; 0; 0; 0; 0; 0|]

// Increment Location
let increment 
    (ccw : bool)
    (loc : Loc * int) 
    (occ : Loc[]) = 
    let occ = Array.append occ [|(fst loc)|]
    match loc with 
    | x,y when y >= 0 -> 
        let inc1 = x 
                |> adjacent ccw
                |> Array.except occ
        let inc2 = match (Array.tryItem  1 inc1) with 
                        | Some a -> 
                                        let bl1 = Array.contains (Array.head inc1) (adjacent ccw a)
                                        match bl1 with 
                                        | true -> Array.tryHead inc1
                                        | false -> x
                                                |>  adjacent ccw 
                                                |> Array.except occ
                                                |> Array.tryLast

                        | None -> Array.tryHead inc1
        match inc2 with 
        | Some a -> a, y
        | None -> (identity,-1)
    | _ -> (identity,-1)

// Get Location from tuple 
let getLocs 
    (loc : (Loc*int)[]) = 
    loc
    |> Array.map(fun x 
                    -> fst x)

// Available Adjacent Locations
let available 
    (loc : (Loc*int))
    (occ : Loc[]) = 
    loc 
    |> fst 
    |> adjacent false
    |> Array.except (Array.append occ [|(fst loc)|] )
    |> Array.length

// Increment Locations
let increments 
    (ccw : bool) 
    (loc : (Loc*int)[]) 
    (occ : Loc[]) = 
    let occ = Array.append occ (getLocs loc)
    let inc = 
        Array.scan (fun ac st -> 
        let occ = (Array.concat [|occ;[|fst st|];[|fst ac|]|] )
        increment ccw st (Array.append[|fst ac|] occ )) 
            loc[0] loc
            |> Array.tail
    inc
(*     // Avoid identical increments
    let bln = Array.map2 (fun x y -> x=y) 
                (Array.map(fun y ->(inc |> Array.findIndex(fun x-> x = y)))inc) 
                ([|0..(Array.length inc)-1|])
                
    let rpt = Array.zip3 loc inc bln
    rpt |> Array.map (fun x -> match x with 
                                | a,b,true -> b
                                | a,b,false -> increment ccw a (Array.append occ (getLocs inc)))
 *)
// Cluster Locations
let clusters 
    (ccw : bool) 
    (loc : (Loc*int)[]) 
    (occ : Loc[]) = 

    let cnt = 
            loc
            |> Array.map (fun x -> snd x)
            |> Array.max
    let acc = Array.chunkBySize 1 loc
    let occ = Array.append occ (getLocs loc)  
    
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
                    |> getLocs
                    |> Array.append occ
                    |> Array.append (getLocs loc)
                    |> Array.distinct

                let rpt = Array.map (fun x 
                                        -> (snd x)-1) loc
                let loc =  
                    acc
                    |> Array.map (fun x
                                    -> Array.filter (fun a 
                                                        -> (available a occ) > 0) x)
                    |> Array.map (fun x 
                                    -> Array.tryHead x)
                    |> Array.map (fun x 
                                    -> match x with
                                        | Some a -> a 
                                        | None -> (identity,-1))                
                    |> Array.map2 (fun x y 
                                    -> fst y,  x) rpt
                
                let inc = increments ccw loc occ
                
                            
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)

                let occ = Array.concat[|getLocs (Array.concat [|Array.concat acc; inc;loc|]);occ|]

                (clsts loc occ acc (cnt-1))
                
    let a = clsts loc occ acc cnt
            |> Array.map(fun x 
                            -> Array.filter(fun (_,z) -> z >= 0) x)
    a     