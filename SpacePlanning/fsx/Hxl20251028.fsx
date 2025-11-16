/// <summary> Hexel is a location representing an irregular hexagonal module.
/// Collections of hexels in a hexagonal grid form Coxels.
/// A hexel can have a maximum of six neighbouring/adjacent hexels.
/// All neighbouring hexels share at least one common edge </summary>
module Hexel =
    /// <summary> Hexel is a location representing an irregular hexagonal module.
    /// Collections of hexels in a hexagonal grid form Coxels.
    ///

    /// <summary> Hexel types: Categorization based on location availabity. </summary>
    ///<typeparam name="AV"> AvaiIable Hexels. </typeparam>
    ///<typeparam name="RV"> Reserved Hexels. </typeparam>
    type Hxl = 
        | AV of x:int * y:int * z:int
        | RV of x:int * y:int * z:int
        | EX of x:int * y:int * z:int
    ///

    /// <summary> Sequence specifies the orientation of hexels, the direction of flow of 
    /// adjacent hexels and the position of the first of the six adjaent hexels. </summary>
    /// <remarks> 
    /// <para>
    /// Horizontal refers to a Flat Top hexagonal grid.
    /// 
    ///  ___ N N ___     ___     ___     ___     ___     ___
    /// /N W\___/N E\___/   \___/   \___/   \___/   \___/   \
    /// \___/   \___/   \___/   \___/   \___/   \___/   \___/
    /// /S W\___/S E\___/   \___/   \___/   \___/   \___/   \
    /// \___/S S\___/   \___/   \___/   \___/   \___/   \___/
    /// 
    /// </para>
    /// <para>
    /// Vertical refers to a Pointy Top hexagonal grid. 
    /// 
    ///   |NW |NE|   |   |   |   |   |   |   |   |   |   |
    ///  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\ 
    /// /  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \
    /// |WW |   |EE |   |   |   |   |   |   |   |   |   |  |
    /// \  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /
    ///  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/  \/ 
    ///   |SW |SE|   |   |   |   |   |   |   |   |   |   |
    /// 
    /// </para>
    /// </remarks>
    /// <typeparam name="VRCWEE"> Orientation:Vertical, Flow:Clockwise, Start:East </typeparam>
    /// <typeparam name="VRCCEE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:East </typeparam>
    /// <typeparam name="VRCWSE"> Orientation:Vertical, Flow:Clockwise, Start:South-East </typeparam>
    /// <typeparam name="VRCCSE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-East </typeparam>
    /// <typeparam name="VRCWSW"> Orientation:Vertical, Flow:Clockwise, Start:South-West </typeparam>
    /// <typeparam name="VRCCSW"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-West </typeparam>
    /// <typeparam name="VRCWWW"> Orientation:Vertical, Flow:Clockwise, Start:West </typeparam>
    /// <typeparam name="VRCCWW"> Orientation:Vertical, Flow:Anti-Clockwise, Start:West </typeparam>
    /// <typeparam name="VRCWNW"> Orientation:Vertical, Flow:Clockwise, Start:North-West </typeparam>
    /// <typeparam name="VRCCNW"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-West </typeparam>
    /// <typeparam name="VRCWNE"> Orientation:Vertical, Flow:Clockwise, Start:North-East </typeparam>
    /// <typeparam name="VRCCNE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-East </typeparam>
    /// <typeparam name="HRCWNN"> Orientation:Horizontal, Flow:Clockwise, Start:North </typeparam>
    /// <typeparam name="HRCCNN"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North </typeparam>
    /// <typeparam name="HRCWNE"> Orientation:Horizontal, Flow:Clockwise, Start:North-East </typeparam>
    /// <typeparam name="HRCCNE"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-East </typeparam>
    /// <typeparam name="HRCWSE"> Orientation:Horizontal, Flow:Clockwise, Start:South-East </typeparam>
    /// <typeparam name="HRCCSE"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-East </typeparam>
    /// <typeparam name="HRCWSS"> Orientation:Horizontal, Flow:Clockwise, Start:South </typeparam>
    /// <typeparam name="HRCCSS"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South </typeparam>
    /// <typeparam name="HRCWSW"> Orientation:Horizontal, Flow:Clockwise, Start:South-West </typeparam>
    /// <typeparam name="HRCCSW"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-West </typeparam>
    /// <typeparam name="HRCWNW"> Orientation:Horizontal, Flow:Clockwise, Start:North-West </typeparam>
    /// <typeparam name="HRCCNW"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-West </typeparam>
    type Sqn =  
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
    ///

    /// <summary> Sequence Locations: Location of adjacent/neighbouring hexels relative to the host hexel.
    /// Each array begins with the location of Host hexel followed by the rest in a particular order.
    /// Hexadecimal number system - 0x0:0, 0x1:1, 0x2:2, 0xFFFFFFFF:-1, 0xFFFFFFFE:-2 </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <returns> An array of two dimensional surrounding locations. </returns>
    let sequence 
        (sqn:Sqn) =  
        match sqn with 
        | VRCWEE -> [|0x0,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2|]
        | VRCCEE -> [|0x0,0x0; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE|]
        | VRCWSE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0|]
        | VRCCSE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE|]
        | VRCWSW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE|]
        | VRCCSW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0|]
        | VRCWWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
        | VRCCWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2|]
        | VRCWNW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0|]
        | VRCCNW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2|]
        | VRCWNE -> [|0x0,0x0; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2|]
        | VRCCNE -> [|0x0,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0|]
        | HRCWNN -> [|0x0,0x0; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1|]
        | HRCCNN -> [|0x0,0x0; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1|]
        | HRCWNE -> [|0x0,0x0; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2|]
        | HRCCNE -> [|0x0,0x0; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF|]
        | HRCWSE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1|]
        | HRCCSE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
        | HRCWSS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF|]
        | HRCCSS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF|]
        | HRCWSW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
        | HRCCSW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1|]
        | HRCWNW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF|]
        | HRCCNW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2|]
    ///

    /// <summary> Identity Hexel. </summary>
    /// <returns> Available (AV) Hexel at global origin. </returns>
    let identity 
        (elv:int) = 
        AV(0x0,0x0, elv)
    ///

    /// <summary> Extract coordinates from hexel. </summary>
    /// <param name="hexel"> Hexel of type AV/RV. </param>
    /// <returns> Tuple of integers representing three dimensional coordinates. </returns>
    let hxlCrd 
        (hxl : Hxl) = 
        match hxl with 
        | AV (a,b,c) -> (a,b,c)
        | RV (a,b,c) -> (a,b,c)
        | EX (a,b,c) -> (a,b,c)
    ///

    /// <summary> Valid Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> Hexel whose coordinates need to be validated. </param> 
    /// <returns> Valid hexel coordinates. </returns>
    let hxlVld 
        (sqn : Sqn)
        (hxl : Hxl) = 
            let validate 
                sqn 
                crx 
                cry 
                crz = 
                    match sqn with
                    | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                        -> match crx,cry with 
                            | a,b when (b%4 = 0) -> (a + (a%2)), b, crz
                            | a,b when (a%2 = 0)-> a+1, (b + (b%2)), crz
                            | _,b-> crx, (b + (b%2)), crz
                    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                        -> match crx,cry with 
                            | a,b when (a%4 = 0) -> a, (b + (b%2)), crz
                            | a,b when (b%2 = 0) -> (a + (a%2)), b+1, crz
                            | a , _->  (a + (a%2)), cry, crz
            // Get hexel coordinayes
            let crx,cry,crz = hxlCrd hxl
            // Validate coordinates
            let x1,y1,z1 = validate sqn crx cry crz
            // Revalidate changed coordinates
            let vld = validate sqn x1 y1 z1
            // Hexels with validated coordinates
            match hxl with
            | AV(_) -> AV(vld)
            | RV(_) -> RV(vld)
            | EX(_) -> EX(vld)
    ///

    /// <summary> Change all hexel types to a uniform type.</summary>
    /// <param name="opt"> 1:AV, 2:RV, 3:EX. </param>
    /// <param name="hxl"> An array of hexels. </param>
    /// <returns> Converts all opted type </returns>
    let hxlUni
        (opt : int)
        (hxl : Hxl[]) = 
        hxl
        |> Array.Parallel.map(fun x -> hxlCrd x)
        |> Array.Parallel.map(fun x -> match opt with 
                                                        | 1 -> AV x
                                                        | 2 -> RV x
                                                        | 3 -> EX x
                                                        | _ -> AV x)
    ///

    /// <summary> Get Hexel from Tuple. </summary>
    /// <param name="hxo"> Tuple containing Base hexel of collection and size. </param>
    let getHxls 
        (hxo : (Hxl*int)[]) = 
        hxo
        |> Array.map(fun x 
                        -> fst x)
    ///

    /// <summary> Adjacent Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Base hexel. </param> 
    /// <returns> An array of six adjacent hexels. </returns>
    let adjacent 
        (sqn : Sqn)
        (hxo : Hxl) =
        match hxo with 
        | AV (x,y,z) -> Array.map 
                            (fun (a,b) -> 
                            AV(x+a, y+b,z))(sequence sqn)
        | RV (x,y,z) -> [|RV(x,y,z)|]
        | EX (x,y,z) -> [|EX(x,y,z)|]
    ///

    /// <summary> Increment Hexel. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Tuple containing Base hexel of collection and size. </param> 
    /// <param name="occ"> Occupied/Unavailable hexels. </param>
    /// <returns> Tuple containing the next hexel and size. </returns>
    let increment 
        (sqn : Sqn)
        (elv : int)
        (hxo : Hxl * int) 
        (occ : Hxl[]) = 
        let occ = Array.concat 
                    [|
                        occ
                        [|(fst hxo)|]
                        [|identity elv|]
                    |] |> hxlUni 1
        match hxo with 
        | x,y when y >= 0x0 -> 
            let inc1 = x 
                    |> adjacent sqn
                    |> Array.except occ
            let inc2 = match (Array.tryItem  1 inc1) with 
                            | Some a -> 
                                            let bl1 = Array.contains (Array.head inc1) (adjacent sqn a)
                                            match bl1 with 
                                            | true -> Array.tryHead inc1
                                            | false -> x
                                                    |>  adjacent sqn 
                                                    |> Array.except occ
                                                    |> Array.tryLast

                            | None -> Array.tryHead inc1
            match inc2 with 
            | Some a -> a, y
            | None -> (hxlVld sqn (identity elv),0xFFFFFFFF)
        | _ -> (hxlVld sqn (identity elv),0xFFFFFFFF)
    ///

    /// <summary> Available Adjacent Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Hexel or Tuple containing Base hexel of collection and size. </param> 
    /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
    /// <returns> The count of unoccupied surrounding hexels. </returns>
    let available 
        (sqn : Sqn)
        (elv : int)
        (hxo : obj)
        (occ : Hxl[]) =  
        let occ = occ |> hxlUni 1
        let hx1 = match hxo with 
                    | :? (Hxl*int) as (a,_) -> a
                    | :? Hxl as b ->  b
                    | _ -> identity elv
        hx1 
        |> adjacent sqn
        |> Array.except 
            (Array.append occ [|hx1|])
        |> Array.length
    ///

    ///<summary> Assign Hexel type. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <returns> Reassigned Hexel Types </returns>
    let hxlChk
        (sqn : Sqn)
        (elv : int)
        (occ : Hxl[])
        (hxl : Hxl[]) = 
        hxl |> Array.map (fun x -> 
                                    match (x = EX(hxlCrd x)) with 
                                    | true -> x
                                    | false -> match (available sqn elv x (Array.append occ hxl)) < 1 with 
                                                | true -> RV(hxlCrd x)
                                                | false -> AV(hxlCrd x))
    ///

    ///<summary> Add Hexel at Narrow Bridge. </summary>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <returns> Thickened List if Bridged </returns>
    let hxlFil
        (sqn : Sqn)
        (elv : int)
        (hxl : Hxl[]) = 

        let hxx = hxl |> hxlUni 1     
        let hx1 = hxx |> Array.map (fun x -> adjacent sqn x)

        let in1 =
            Array.map (fun z ->
                Array.map (fun y ->
                    Array.tryFindIndex (fun x -> x = y) z
                ) hxx
            ) hx1

        let in2 =
            in1
            |> Array.map (fun x -> x |> Array.choose id)
            |> Array.map Array.sort
            |> Array.map (fun x -> Array.except [|0|] x)

        // safe in3
        let in3 =
            in2
            |> Array.map (fun x ->
                match x |> Array.toList with
                | [] -> [||]
                | [a] -> [| a |]
                | a :: _ ->
                    let lastVal = Array.last x
                    [| a .. lastVal |])

        // safe in4
        let in4 =
            match in2, in3 with
            | a, b when a.Length = b.Length ->
                Array.map2 (fun x y -> Array.except x y) a b
            | _ -> [||]

        // safe in5
        let in5 =
            in4
            |> Array.map (fun x ->
                match x |> Array.toList with
                | [] -> [||]
                | [_] -> x
                | _ ->
                    let first = Array.head x
                    let last  = Array.last x
                    Array.except x [| first .. last |])

        // safe hx2
        let hx2 =
            match hx1, in5 with
            | a, b when a.Length = b.Length ->
                Array.map2 (fun x y ->
                    match Array.toList x, Array.tryHead y with
                    | [], _ -> [||]
                    | _, None -> [||]
                    | _, Some yi when yi >= 0 && yi < x.Length ->
                        [| Array.get x yi |]
                    | _ -> [||]
                ) a b
                |> Array.concat
            | _ -> [||]

        Array.append hxx hx2 |> hxlChk sqn elv [||]

    ///

    /// <summary> Increment Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Array of Tuples containing Base hexel of collection and size. </param> 
    /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
    /// <returns> Array of Tuples containing Base hexel of collection and reduced size. </returns>
    let increments 
        (sqn : Sqn)
        (elv : int)
        (hxo : (Hxl*int)[]) 
        (occ : Hxl[]) = 
        let occ = (Array.append occ (getHxls hxo)) |> hxlUni 1
        let inc = 
            Array.scan (fun ac st -> 
            let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity elv|]|]) |> hxlUni 1
            increment sqn elv st (Array.append[|fst ac|] occ )) 
                hxo[0] hxo
                |> Array.tail
        ///
        
        /// <summary> Generate alternate hexel in cases where there are overlapping hexels </summary>
        /// <param name="sqn"> Sequence to follow. </param>
        /// <param name="hxo"> Array of Tuples containing Base hexel of collection and size. </param> 
        /// <param name="hxo"> Array of Tuples containing Incremental hexel of collection and reduced size. </param>
        /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
        /// <returns> Array of Tuples containing alternate incremental hexel of collection and reduced size. </returns>
        let replaceDuplicate 
            (sqn : Sqn)
            (hxo : (Hxl*int)[]) 
            (inc : (Hxl*int)[]) 
            (occ : Hxl[]) =   
            let in1 = Array.map (fun x -> snd x)inc
            let lc1 = getHxls hxo 
            let ic1 = getHxls inc 
            let oc1 = Array.concat[|occ;lc1;ic1|] |> hxlUni 1
            let id1 = Array.map(fun y -> Array.findIndex (fun x -> x = y)ic1)ic1
            let bl1 = Array.map2 (fun x y -> x=y) [|(0x0)..(Array.length ic1)-(0x1)|] id1   
            let tp1 = Array.zip3 bl1 ic1 hxo  
            tp1 |> Array.map2 (fun d (a,b,c) 
                                -> match a with 
                                    | true -> b,d
                                    | false -> 
                                            match ((available sqn elv c oc1) > 0x0) with 
                                            | false -> (fst c),0xFFFFFFFF
                                            | true -> fst(increment sqn elv c oc1),d) in1
            
        replaceDuplicate sqn hxo inc occ
    ///

    /// <summary> Boundary Hexels Ring. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <returns> Boundary/Peripheral hexels. </returns>
    let bndSqn
        (sqn : Sqn)
        (elv : int)
        (hxo : Hxl[]) = 
        /// <summary> Arrange/sort hexels in continuous sequence. </summary>
        /// <param name="sqn"> Sequence to follow. </param>
        /// <param name="hxl"> Array of hexels. </param>
        /// <param name="acc"> Accumulator for recursive function. </param>
        /// <param name="cnt"> Counter. </param>
        /// <returns> Array of sorted hexels </returns>
        let rec arr 
            (sqn : Sqn)
            (elv : int)
            (hxl : Hxl[]) 
            (acc : Hxl[]) 
            (cnt : int)
            (opt : bool) = 
            match cnt with 
            | a when cnt <= 0x1 -> acc
            | _ -> 
                let hxl = Array.except acc hxl
                let hx1 = ((Array.filter (fun x -> Array.contains x hxl) 
                                (adjacent sqn (Array.last acc))))                
                let hx2 = match opt with 
                                | false -> Array.tryHead hx1
                                | true -> Array.tryLast hx1
                let hx3 = match hx2 with 
                                | Some a -> [|a|]
                                | None -> [||]
                let acc = Array.append acc  hx3
                arr sqn elv hxl acc (cnt-1) opt

        let hxl = hxo
                |> Array.sortByDescending 
                    (fun x -> available sqn elv x hxo)
        let a1 = 
            match hxl with 
            | [||] -> [||]
            | _ -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) true

        let b1 = (Array.length a1) = Array.length hxl
            
        let ar1 = match b1 with 
                    | true -> a1
                    | false -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) false
        let ar2 = 
            match hxo with 
            | [||] -> [||]
            | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                    | true -> ar1
                    | false -> hxlUni 2 ar1
            
        // Arrange clockwise
        let ar3 = Array.windowed 2 ar2
        let bln = Array.map(fun x 
                                ->  let cdx1,cdy1,_ = hxlCrd (Array.head x)
                                    let cdx2,cdy2,_ = hxlCrd (Array.last x)
                                    (cdx2 - cdx1 >= 0) && (cdy1 - cdy2 >= 0)) ar3
        match Array.contains false bln with
        | true -> Array.rev ar2
        | false -> ar2
    ///

    /// <summary> Hexel Ring Segment Sequence. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    let cntSqn
        (sqn : Sqn)
        (elv : int)
        (hxo : Hxl[]) =      
        let hxl = hxlUni 1 hxo
        let rec ctSq 
            (sqn : Sqn)
            (hxl : Hxl[])
            (acc : Hxl[])
            (cnt : int) = 
            match cnt with 
            | x when x<=1 -> acc
            | _ -> 
                    let b = Array.last acc
                    let hxl = Array.except [|b|] hxl
                    let d = (adjacent sqn b) |> Array.tail
                    let e = d |> Array.filter
                                (fun x -> Array.contains x hxl) 
                                |> Array.tryHead
                    let f = match e with 
                                | Some a -> [|a|]
                                | None -> [||]
                    let acc = Array.append acc f
                    ctSq sqn hxl acc (cnt-1)

        let hxl = hxl |> Array.sortByDescending 
                    (fun x -> available sqn elv x hxl)
        let cnt = Array.length(hxl)
        let arr =  match hxl with 
                        | [||] -> [||]
                        | _ -> ctSq sqn hxl ([|Array.head hxl|]) cnt
        let bln = cnt = Array.length(arr)
        let ar1 = match bln with 
                    | true -> arr
                    | false -> ctSq sqn (Array.rev hxl) ([|Array.last hxl|]) cnt
        match hxo with 
        | [||] -> [||]
        | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                | true -> ar1
                | false -> hxlUni 2 ar1
    ///

    /// <summary> Hexel Ring Offset. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <returns> Offset Boundary/Peripheral hexels. </returns>
    let hxlOfs
        (sqn : Sqn)
        (elv : int)
        (hxl : Hxl[]) = 
        hxl 
        |> hxlUni 1 
        |> Array.map(fun x -> adjacent sqn x) 
        |> Array.concat 
        |> Array.distinct 
        |> Array.except (hxlUni 1 hxl)
        |> cntSqn sqn elv
    ///

    /// <param name="org"> All constituent hexels. </param>
    /// <param name="hxl"> Subset of hexels. </param>
    /// <returns> Restored Hexel Types </returns>
    let hxlRst
        (org : Hxl[])
        (hxl : Hxl[]) =
        let crd = Array.map (fun x -> hxlCrd x) hxl
        org |> Array.filter (fun x -> (crd|> Array.contains (hxlCrd x)))
module Coxel =
    open Hexel
    ///

    /// <summary> Coxels are primarily a collections of unique hexels </summary>
    /// <summary> Coxel type properties </summary>
    /// <typeparam name="Label"> Name </typeparam>
    /// <typeparam name="Refid"> Unique reference ID </typeparam>
    /// <typeparam name="Count"> ANumber of hexels in Coxel </typeparam>
    type Prp = 
        | Label of string
        | Refid of string
        | Count of int
    ///

    /// <summary> Coxel type consists of hexels and properties. </summary>
    /// <typeparam name="Name"> Coxel Name. </typeparam>
    /// <typeparam name="Rfid"> Reference ID. </typeparam>
    /// <typeparam name="Size"> Number of hexels. </typeparam>
    /// <typeparam name="Seqn"> Sequence of hexel arrangement. </typeparam>
    /// <typeparam name="Base"> Base hexel. </typeparam>
    /// <typeparam name="Hxls"> Constituent Hexels. </typeparam>
    type Cxl = 
        {
            Name : Prp
            Rfid : Prp
            Size : Prp
            Seqn : Sqn
            Base : Hxl
            Hxls : Hxl[]
        }  
    ///

    /// <summary> Property value types </summary>
    /// <typeparam name="Label">  Name of coxel. </typeparam>
    /// <typeparam name="Refid">  Reference ID. </typeparam>
    /// <typeparam name="Count">  Number of hexels as a string. </typeparam>
    let prpVlu 
        (prp : Prp) = 
        match prp with 
        | Label prp -> prp
        | Refid prp -> prp
        | Count prp -> prp.ToString()
    ///

    /// <summary> Creating an array of coxels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="ini"> An array of tuples containing base hexel, Reference Id, Count/Size, Label. </param>
    /// <param name="occ"> Hexels that are unavailable. </param>
    /// <returns> An array of coxels. </returns>
    let coxel
        (sqn : Sqn)
        (elv : int)
        (ini : (Hxl*Prp*Prp*Prp)[])
        (occ : Hxl[]) = 
            
        let bas = Array.Parallel.map(fun (x,_,y,_) -> x,int(prpVlu y)) ini
        let szn = Array.Parallel.map(fun (_,_,y,z) -> y,z) ini
        let idn = Array.Parallel.map (fun(x,y,_,_)-> x,y) ini

        let cnt = 
                bas
                |> Array.Parallel.map (fun x -> snd x)
                |> Array.max
        let acc = Array.chunkBySize 1 bas
        let oc1 = (Array.append occ (getHxls bas)) |> hxlUni 1 
            
        let rec clsts 
            (hxo: (Hxl*int)[])
            (elv : int)
            (occ : Hxl[])
            (acc:(Hxl*int)[][])
            (cnt : int)= 
                
            match cnt with 
            | c when c < 0x1 -> acc
            | _ -> 
                    let occ = 
                        acc 
                        |> Array.concat 
                        |> getHxls
                        |> Array.append occ
                        |> Array.append (getHxls hxo)
                        |> Array.distinct
                        |> hxlUni 1

                    let rpt = Array.Parallel.map (fun x 
                                                    -> (snd x) - 0x1) hxo
                    let hx1 =  
                        acc
                        |> Array.Parallel.map (fun x
                                                -> Array.filter (fun a -> (available sqn elv a occ) > 0x0) x)
                        |> Array.Parallel.map (fun x 
                                                -> Array.tryHead x)
                        |> Array.Parallel.map (fun x 
                                                -> match x with
                                                    | Some a -> a 
                                                    | None ->  (hxlVld sqn (RV(0,0,elv)),0xFFFFFFFF))                
                        |> Array.map2 (fun x y 
                                        -> fst y, x) rpt
                        
                    let inc = increments sqn elv hx1 occ
                                
                    let acc = Array.map2  (fun x y
                                            -> Array.append x y) 
                                acc
                                (Array.chunkBySize 1 inc)
                    // Avoid bridge in Coxel
                    //let acc = ac1 
                    //            |> Array.Parallel.map (fun x -> match Array.length x > 3 with
                    //                                            | false -> x
                    //                                            | true -> 
                    //                                                    let h1 = hxlUni 1 (getHxls x) 
                    //                                                    let h2 = h1.[Array.length h1 - 2]
                    //                                                    let b1 = available sqn h2 (Array.except [|h2; Array.last h1|] h1) = 5
                    //                                                    let b2 = available sqn (Array.last h1) (h1 |> Array.rev |> Array.tail) = 5
                    //                                                    match b1 && b2 with 
                    //                                                    | false -> x
                    //                                                    | true -> Array.removeManyAt (Array.length h1 - 2) 2 x)

                    let occ = Array.concat[|getHxls 
                        (Array.concat [|
                        Array.concat acc
                        inc
                        hx1|]);occ|] 
                            |> hxlUni 1

                    (clsts hx1 elv occ acc (cnt - 0x1))
            
        let cls = 
            clsts bas elv oc1 acc cnt
                |> Array.Parallel.map(fun x 
                                        -> Array.filter(fun (_,z) -> z >= 0) x)
            
        let cl1 = 
            cls
            |> Array.Parallel.map(fun x -> getHxls x)

    (*  // Avoid single unclustered cell towards the end
        let hxlElm (sqn:Sqn) (hxl:Hxl[])=
            let hxo = hxl
            let avl = 5
            let hxl = hxlUni 1 hxl
            let acc = hxl |> Array.filter (fun x -> (available sqn x hxl) < avl)
            let rec elm (sqn:Sqn) (hxl:Hxl[]) (acc: Hxl[]) = 
                let hx1 = hxl
                match (Array.length hx1 = Array.length acc) with
                | true -> acc
                | false -> 
                            let hx1 = acc
                            let acc = hx1|> Array.filter (fun x -> (available sqn x hx1) < avl)
                            elm sqn hx1 acc
            let hx1 = elm sqn hxl acc
            hxlRst hxo hx1

    let cl01 = 
            cl00 |> Array.Parallel.map(fun x -> hxlElm sqn x)
            //|> Array.Parallel.map(fun x -> x |> hxlFil sqn)

        //let cl01 = cl00 |> Array.Parallel.map(fun x -> Array.filter(fun y -> (available sqn y x) < 5)x)
    
        let cl1 = Array.map2 (fun x y 
                                    -> Array.append [|Array.head x|] y) cl00 cl01*)

        let cxl =
            Array.map3 (fun x y z ->
                // z -> candidate hexel cluster (Hxl[])
                // Ensure we get a safe hx1 from hxlChk
                let hx1 = z |> hxlChk sqn elv (Array.append occ z)

                // pattern-match hx1 so we never call Array.head on empty array
                match Array.toList hx1 with
                | [] ->
                    // fallback when hx1 is empty: choose defaults
                    {
                        Name = snd x
                        Rfid = snd y
                        Size = fst x
                        Seqn = sqn
                        Base = identity elv           // fallback base
                        Hxls = [||]               // no hexels available
                    }
                | head :: _ ->
                    // hx1 is non-empty, safe to use head and build Hxls
                    let rest =
                        match Array.length hx1 > 0 with
                        | true -> Array.except ([| head; identity elv |]) hx1
                        | false -> [||] // unreachable because of match, but kept for clarity

                    {
                        Name = snd x
                        Rfid = snd y
                        Size = fst x
                        Seqn = sqn
                        Base = head
                        Hxls = rest
                    }
            ) szn idn cl1

        cxl
    ///

    /// <summary> Count open/exposed Hexels. </summary>
    /// <param name="cxl"> A coxel. </param>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
    let cxlExp 
        (cxl : Cxl[])
        (sqn : Sqn)
        (elv : int)= 
        let occ = cxl |> Array.map (fun x -> x.Hxls) |> Array.concat |> hxlUni 1 
        let cxlAvl 
            (cx:Cxl)
            (sq:Sqn)
            (oc:Hxl[]) =
            let hx = cx.Hxls |> hxlUni 1 
            hx |> Array.filter(fun x -> (available sq elv x oc)>0) |> Array.length
        cxl |> Array.map (fun a -> cxlAvl a sqn occ)
    ///

    /// <summary> Categorize constituent Hexels within a Coxel. </summary>
    /// <param name="cxl"> A coxel. </param>
    /// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
    let cxlHxl
        (cxl : Cxl)
        (elv : int) = 
        /// <summary> Hexel Ring Boundary Sequence. </summary>
        /// <param name="sqn"> Sequence to follow. </param>
        /// <param name="hxl"> All constituent hexels. </param>
        /// <returns> Boundary/Peripheral hexels. </returns>
        let bndSqn
            (sqn : Sqn)
            (elv : int)
            (hxo : Hxl[]) = 
            /// <summary> Arrange/sort hexels in continuous sequence. </summary>
            /// <param name="sqn"> Sequence to follow. </param>
            /// <param name="hxl"> Array of hexels. </param>
            /// <param name="acc"> Accumulator for recursive function. </param>
            /// <param name="cnt"> Counter. </param>
            /// <returns> Array of sorted hexels </returns>
            let rec arr 
                (sqn : Sqn)
                (elv : int)
                (hxl : Hxl[]) 
                (acc : Hxl[]) 
                (cnt : int)
                (opt : bool) = 
                match cnt with 
                | a when cnt <= 0x1 -> acc
                | _ -> 
                    let hxl = Array.except acc hxl
                    let hx1 = ((Array.filter (fun x -> Array.contains x hxl) 
                                    (adjacent sqn (Array.last acc))))                
                    let hx2 = match opt with 
                                    | false -> Array.tryHead hx1
                                    | true -> Array.tryLast hx1
                    let hx3 = match hx2 with 
                                    | Some a -> [|a|]
                                    | None -> [||]
                    let acc = Array.append acc  hx3
                    arr sqn elv hxl acc (cnt-1) opt

            let hxl = hxo|> Array.sortByDescending 
                        (fun x -> available sqn elv x hxo)
            let a1 = 
                match hxl with 
                | [||] -> [||]
                | _ -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) true

            let b1 = Array.length a1 = Array.length hxl
                
            let ar1 = match b1 with 
                        | true -> a1
                        | false -> arr sqn elv hxl [|Array.last hxl|] (Array.length hxl) false
            match hxo with 
            | [||] -> [||]
            | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                    | true -> Array.rev ar1
                    | false -> Array.rev (hxlUni 1 ar1)

        /// <summary> Hexel Ring Segment Sequence. </summary>
        let cntSqn
            (sqn : Sqn)
            (elv : int)
            (hxo : Hxl[]) =      
            let hxl = hxlUni 1 hxo
            let rec ctSq 
                (sqn : Sqn)
                (hxl : Hxl[])
                (acc : Hxl[])
                (cnt : int) = 
                match cnt with 
                | x when x<=1 -> acc
                | _ -> 
                        let b = Array.last acc
                        let hxl = Array.except [|b|] hxl
                        let d = (adjacent sqn b) |> Array.tail
                        let e = d |> Array.filter
                                    (fun x -> Array.contains x hxl) 
                                    |> Array.tryHead
                        let f = match e with 
                                    | Some a -> [|a|]
                                    | None -> [||]
                        let acc = Array.append acc f
                        ctSq sqn hxl acc (cnt-1)

            let hxl = hxl |> Array.sortByDescending 
                        (fun x -> available sqn elv x hxl)
            let cnt = Array.length(hxl)
            let arr =  match hxl with 
                            | [||] -> [||]
                            | _ -> ctSq sqn hxl ([|Array.head hxl|]) cnt
            let bln = cnt = Array.length(arr)
            let ar1 = match bln with 
                        | true -> arr
                        | false -> ctSq sqn (Array.rev hxl) ([|Array.last hxl|]) cnt
            match hxo with 
            | [||] -> [||]
            | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                    | true -> ar1
                    | false -> hxlUni 2 ar1

        let avrv = cxl.Hxls 
                |> Array.Parallel.partition
                    (fun x -> x = AV(hxlCrd x))
        let rv01 = (snd avrv) 
                |> Array.Parallel.partition
                    (fun x-> (available 
                        cxl.Seqn
                        elv
                        (AV(hxlCrd x)) 
                        (hxlUni 1 (cxl.Hxls))) < 1)
        let av01 = match (snd rv01) with 
                    | [||] -> avrv |> fst |> bndSqn cxl.Seqn elv
                    | _ -> avrv |> fst |> cntSqn cxl.Seqn elv
        let br01 = match (fst rv01) with 
                    | [||] -> rv01 |> snd |> bndSqn cxl.Seqn elv
                    | _ -> rv01 |> snd |> cntSqn cxl.Seqn elv
            
        let pr01 = match av01 with 
                        | [||] -> br01
                        | _ -> match br01 with 
                                | [||] -> av01
                                | _ -> match adjacent 
                                        cxl.Seqn 
                                        (Array.last av01) 
                                        |> hxlUni 2
                                        |> Array.contains (Array.head br01) with 
                                        | true -> Array.append av01 br01
                                        | false -> Array.append av01 (Array.rev br01)
        // Clockwise sequence
        let pr02 = match Array.length pr01 > 2 with 
                        | false -> pr01
                        | true -> 
                                let x1,y1,_ = hxlCrd (Array.last pr01)
                                let x2,y2,_ = hxlCrd (Array.head pr01)
                                let x3, y3,_ = hxlCrd (pr01[1])
                                let gs = (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
                                match gs with 
                                | 0 when x2 > x1 -> pr01
                                | a when a < 0 -> pr01
                                | _ -> Array.rev pr01

        {|
            Base = cxl.Base
            Hxls = cxl.Hxls
            Core = rv01 |> fst 
            Prph = pr02 
            Brdr = br01
            Avbl = av01 
        |}  

module Geometry =
    open Hexel
    open Coxel
    open System
    ///

    /// <summary> Module shape in tessalated hexagonal grid. </summary>
    /// <typeparam name="Hxg"> Hexagon. </typeparam>
    /// <typeparam name="Sqr"> Square. </typeparam>
    /// <typeparam name="Arw"> Arrow. </typeparam>
    /// <typeparam name="Prl"> Parallelogram. </typeparam>
    type Shp = 
        | Hxg | Sqr | Arw | Prl

    // Hexel Vertices
    let vertex
        (sqn : Sqn)
        (shp : Shp)
        (hxl : Hxl) = 
        let hxCr = 
            match shp with 
            | Hxg 
                -> match sqn with 
                    |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
                        -> [|0x0,0x0; 0x1,0x1; 0x2,0x0; 0x2,0xFFFFFFFF; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFF|]
                    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                        -> [|0x0,0x0; 0x1,0x0; 0x2,0xFFFFFFFF; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFF|]
            | Sqr 
                -> match sqn with 
                    |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                        -> [|0x0,0x0; 0x1,0x0; 0x2,0x0; 0x2,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE|]
                    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                        -> [|0x0,0x0; 0x2,0x0; 0x2,0xFFFFFFFF; 0x2,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0x0,0xFFFFFFFF|]
            | Arw 
                -> match sqn with 
                    |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                        -> [|0x0,0x0; 0x1,0xFFFFFFFF; 0x2,0x0; 0x2,0xFFFFFFFD; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFD|]
                    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                        -> [|0x0,0x0; 0x3,0x0; 0x2,0xFFFFFFFF; 0x3,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0x1,0xFFFFFFFF|]
            | Prl 
                -> match sqn with 
                    |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                        -> [|0x0,0x0; 0x1,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
                    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                        -> [|0x0,0x0; 0x2,0x1; 0x2,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x0,0xFFFFFFFF|] 
                    
        let x, y, _ = hxl |> hxlCrd 
        hxCr 
        |> Array.map(fun (a,b)-> a + x, b + y) 
        |> Array.map2 ( fun inx (vrx,vry) 
                            -> string(inx),vrx,vry) 
                            [|0..(Array.length hxCr)-1|]
    ///

    /// <summary> Ortogonal Hexel Sequence </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="org"> Start Hexel. </param> 
    /// <param name="lgt"> Sequence Length. </param> 
    /// <param name="vrt"> Vertical or Horizontal Orientation. </param> 
    /// <param name="rev"> Positive or Negative Direction. </param> 
    /// <returns> Array of Sequential Reserved Hexels. </returns>
    let hxlOrt
            (sqn : Sqn)
            (org : Hxl)
            (lgt : int)
            (vrt : bool)
            (rev : bool) =
            let hxx,hxy,hxz = org |> hxlVld sqn |> hxlCrd
            let lgt = lgt + (lgt%2)
            let sgn = match rev with 
                        | true -> -1
                        | false -> 1         
            match sqn with
            | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                -> match vrt with 
                    | true -> [|hxy..4*sgn..(hxy+lgt+4)*sgn|]
                            |> Array.map (fun y -> [|EX(hxx,y,hxz);EX(hxx+1,y+2*sgn,hxz)|])
                            |> Array.concat
                            |> Array.take ((lgt/2)+1)
                    | false -> [|hxx..2*sgn..(hxx+lgt+4)*sgn|]
                            |> Array.map (fun x -> EX (x,hxy,hxz)) 
                            |> Array.take ((lgt/2)+1)
            | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                -> match vrt with
                    | true -> [|hxy..2*sgn..(hxy+lgt)*sgn|]
                            |> Array.map (fun y -> RV (hxx,y,hxz)) 
                            |> Array.take ((lgt/2)+1)
                    | false -> [|hxx..4*sgn..(hxx+lgt)*sgn|] 
                            |> Array.map (fun x -> [|EX(x,hxy,hxz);EX(x+2*sgn,hxy+1,hxz)|])
                            |> Array.concat
                            |> Array.take ((lgt/2)+1)

    let hxlOff
        (hxl : Hxl[])
        (rev : bool) = 
        let neg = match rev with 
                    | true -> -1
                    | false -> 1
        let crd = Array.map (fun a -> hxlCrd a) hxl
        let x1,y1,_ = Array.head crd
        let x2,y2,_ = Array.get crd 1
        match (x1 = x2) with 
        | true -> Array.map(fun (x,y,z) -> EX(x+(2*neg),y-1,z)) crd
        | false -> match (y1=y2) with 
                    | true -> Array.map(fun (x,y,z) -> EX(x+1,y+(2*neg),z)) crd
                    | false -> match (Math.Abs(x2-x1)=1) with 
                                | true -> Array.map(fun (x,y,z) -> EX(x+(2*neg),y,z)) crd
                                | false -> Array.map(fun (x,y,z) -> EX(x,y+(2*neg),z)) crd

    let hxlRct
        (sqn : Sqn)
        (wdt : int)
        (hgt : int)
        (str : int) = 
        let sqn = match sqn with
                    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW-> HRCWNN
                    | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> VRCWEE
        let org = hxlVld sqn (AV(0,0,0))
        let hrz1 = hxlOrt sqn org ((wdt+1)*2) false false
        let vrt1 = hxlOrt sqn (Array.last hrz1) ((hgt+1)*2) true false
        let hrz2 = hxlOrt sqn (Array.last vrt1) ((wdt+1)*2) false true
        let vrt2 = hxlOrt sqn (Array.last hrz2) ((hgt+1)*2) true true

        let bs1 = match str with 
                    | 1 -> Array.get hrz1 ((Array.length hrz1)/2)
                    | 2 -> vrt1 |> Array.tail |> Array.head
                    | 3 -> Array.get vrt1 ((Array.length vrt1)/2)
                    | 4 -> hrz2 |> Array.tail |> Array.head
                    | 5 -> Array.get hrz2 ((Array.length hrz2)/2)
                    | 6 -> vrt2 |> Array.tail |> Array.head
                    | 7 -> Array.get vrt2 ((Array.length vrt2)/2)
                    | _ -> hrz1 |> Array.tail |> Array.head

        let rct = Array.concat[|hrz1; vrt1; hrz2; vrt2; 
                        hxlOff hrz1 true; 
                        hxlOff vrt1 false; 
                        hxlOff hrz2 false; 
                        hxlOff vrt2 true|] 
                        |> Array.distinct
        let bas = AV(hxlCrd bs1) |> adjacent sqn |> hxlUni 3 |> Array.except rct |> Array.head
        AV(hxlCrd bas),rct

    ///

    /// <summary> Hexel Line </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="stt"> Start Hexel. </param> 
    /// <param name="enn"> End Hexel. </param> 
    /// <returns> Array of Sequential Reserved Hexels. </returns>

    let hxlLin 
        (sqn: Sqn)
        (elv : int) 
        (stt: Hxl) 
        (enn: Hxl) =
        let safeHxlCrd (hOpt: Hxl option) =
            match hOpt with
            | Some h -> hxlCrd h   // valid input
            | None -> 0, 0, elv    // fallback for empty/invalid input
        let sx, sy, sz = safeHxlCrd (Some stt)
        let ex, ey, _ = safeHxlCrd (Some enn)

        let splitOddChunks (n: int) (arr: 'T[]) : 'T[][] =
            let len = arr.Length

            let rec loop i start acc =
                match i with
                | i when i >= n -> acc |> List.rev |> Array.ofList
                | _ ->
                    let remaining = len - start
                    let remainingChunks = n - i

                    // ideal fair size
                    let fairSize = remaining / remainingChunks

                    // adjust to odd (unless it's the last chunk)
                    let size =
                        match i = n - 1 with
                        | true -> remaining
                        | false ->
                            match fairSize % 2 with
                            | 0 when remaining > remainingChunks -> fairSize + 1
                            | _ -> fairSize

                    let chunk = arr.[start .. start + size - 1]
                    loop (i + 1) (start + size) (chunk :: acc)

            loop 0 0 []

        let dropAlternate (arr) =
            arr
            |> Array.mapi (fun i x -> i, x)
            |> Array.choose (fun (i, x) -> if i % 2 = 0 then Some x else None)

        let bumpEveryOther (hr:bool) (arr: (int * int)[]) =
            arr
            |> Array.mapi (fun i (x, y) ->
                match i % 2 with
                | 0 -> (x, y)
                | _ -> match hr with
                        | true -> (x, y + 1)
                        | false -> (x + 1 , y)  
            )

        let seqH = 
            match sqn with
            | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> false
            | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW -> true

        let pty,flt =  
            let pth = match sx <= ex with
                        | true -> [|sx .. 2 .. ex|] 
                        | false -> [|sx .. -2 .. ex|]
            let flh = match sy <= ey with
                        | true -> [|sy .. 1 .. ey|]
                        | false -> [|sy .. -1 .. ey|]
            let ptv = match sy <= ey with
                        | true -> [|sy .. 2 .. ey|]
                        | false -> [|sy .. -2 .. ey|]
            let flv = match sx <= ex with
                        | true -> [|sx .. 1 .. ex|]
                        | false -> [|sx .. -1 .. ex|]  
                                
            match seqH with
            |false -> ptv,flv
            |true -> pth,flh
                
        let div = 
            match Array.length pty >= Array.length flt with  
            | true -> 
                let cnk1 = splitOddChunks (Array.length flt) pty
                let a =
                    match seqH with
                    | true -> // horizontal: (b, a)
                        Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (b, a))) flt
                    | false -> // vertical: (a, b)
                        Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (a, b))) flt
                a |> Array.map (bumpEveryOther seqH) |> Array.concat
            | false -> 
                let cnk1 = splitOddChunks (Array.length pty) flt
                let a =
                    match seqH with
                    | true -> // horizontal: (a, b)
                        Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (a, b))) pty
                    | false -> // vertical: (b, a)
                        Array.mapi (fun i a -> cnk1.[i] |> Array.map (fun b -> (b, a))) pty
                a |> Array.map dropAlternate  |> Array.concat

        let result =
            div |> Array.map (fun (a,b) -> RV(a,b,sz))

        match result.Length = 0 with
        | true -> [| stt; enn |] |> Array.distinct 
        | false -> result

    ///

    let removeSawtooth (sqn : Sqn) (arr : (int*int)[]) : (int*int)[] =
        // Determine primary axis
        let vert =
            match sqn with
            | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW
            | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> true
            | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS
            | HRCWSW | HRCCSW | HRCWNW | HRCCNW -> false

        let primary = if vert then snd else fst
        let secondary = if vert then fst else snd

        // Split array by Δ=2 along primary axis
        let splitByDelta2 (arr: (int*int)[]) : (int*int)[][] =
            match arr with
            | [||] -> [||]
            | _ ->
                let folder (groups, currGroup) p =
                    match currGroup with
                    | [||] -> (groups, [|p|])
                    | _ ->
                        let last = currGroup.[currGroup.Length - 1]
                        match abs (primary p - primary last) with
                        | 2 -> (groups, Array.append currGroup [|p|])
                        | _ -> (Array.append groups [|currGroup|], [|p|])
                let groups, lastGroup = Array.fold folder ([||], [||]) arr
                Array.append groups [|lastGroup|]

        // Check secondary-axis oscillation using recursion + pattern matching
        let rec oscillates (values: int[]) idx =
            match idx >= values.Length - 2 with
            | true -> true
            | false ->
                match abs (values.[idx+1] - values.[idx]), abs (values.[idx+2] - values.[idx+1]) with
                | 1, 1 -> oscillates values (idx + 1)
                | _ -> false

        // Post-process subarrays for oscillation
        let processOscillation (subarrays: (int*int)[][]) : (int*int)[][] =
            subarrays
            |> Array.map (fun arr ->
                match arr.Length with
                | n when n <= 2 -> arr
                | _ ->
                    let values = arr |> Array.map secondary
                    match oscillates values 0 with
                    | true ->
                        let first = arr.[0]
                        let last  = arr.[arr.Length - 1]
                        let low = min (secondary first) (secondary last)
                        match vert with
                        | true  -> [| (low, snd first); (low, snd last) |]
                        | false -> [| (fst first, low); (fst last, low) |]
                    | false -> arr
            )

        // --- Pipeline ---
        arr
        |> splitByDelta2
        |> processOscillation
        |> Array.collect id 

    /// <summary> Hexel Polygon </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="vtx"> Integer coordinates of polygon vertices. </param> 
    /// <param name="elv"> Elevation/Level/Z. </param> 
    /// <returns> Array of Sequential Reserved Hexels. </returns>

    let hxlPgn 
        (sqn: Sqn) 
        (elv: int)
        (vtx: (int * int)[]): Hxl[] =
            
        let toHxl (x, y) = hxlVld sqn (RV(x, y, elv))

        match vtx with
        | [||] | [| _ |] -> [||]
        | _ ->
            let verts =
                if vtx.[0] = vtx.[vtx.Length - 1] then vtx
                else Array.append vtx [|vtx.[0]|]

            let edges =
                verts
                |> Array.pairwise
                |> Array.collect (fun ((x1, y1), (x2, y2)) ->
                    let stt, enn = toHxl (x1, y1), toHxl (x2, y2)
                    let seg = hxlLin sqn elv stt enn
                    if seg.Length = 0 then [|stt; enn|] else seg
                )

            edges |> Array.distinct
        
    // Polygon Area
    /// Signed area of a polygon using pattern matching (shoelace formula)
    let polygonArea (poly: (int * int)[]) =
        match poly with
        | [||] | [| _ |] | [| _; _ |] ->
            // Too few points to form a polygon
            0.0
        | pts ->
            let rec loop acc i =
                match i with
                | n when n = pts.Length ->
                    // Close the loop (last to first)
                    let (x1, y1) = pts.[n - 1]
                    let (x2, y2) = pts.[0]
                    acc + (x1 * y2 - x2 * y1)
                | _ ->
                    let (x1, y1) = pts.[i]
                    let (x2, y2) = pts.[(i + 1) % pts.Length]
                    loop (acc + (x1 * y2 - x2 * y1)) (i + 1)
            float (abs (loop 0 0)) / 2.0


    // Polygon Area with Holes
    let polygonWithHolesArea (outer: (int * int)[]) (holes: (int * int)[][]) =
        match outer, holes with
        | [||], _ -> 0.0
        | _, [||] -> polygonArea outer
        | outerPts, holePolys ->
            let outerArea = polygonArea outerPts
            let holesArea =
                holePolys
                |> Array.sumBy (fun hole ->
                    match hole with
                    | [||] | [| _ |] | [| _; _ |] -> 0.0
                    | _ -> polygonArea hole)
            outerArea - holesArea

module Parse =
    open Hexel
    open Coxel
    open Geometry
    open System

    // Sample Space Program Input Format
    let spaceStr =
        "(1/15/Foyer),(2/20/Living),(3/20/Dining),
        (4/20/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
        (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
        (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
        (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"
    ///

    /// <summary> Categorize constituent Hexels within a Coxel. 
    ///</summary>
    /// <param name="spaceStr"> Properly formatted string (RefId,Count,Lablel) </param>
    /// <returns> Array of string arrays (RefId as string * Count as int * Label as string)  </returns>
    /// <summary>
    /// Categorize constituent Hexels within a Coxel (safe + pattern matching version)
    /// </summary>
    let spaceSeq 
        (spaceStr:string) = 
        let splitTopLevel (s: string) : string[] =
            let rec loop (acc: string list) (curr: string) (depth: int) (chars: char list) =
                match chars with
                | [] -> List.rev (curr :: acc)
                | c::cs ->
                    match c with
                    | '(' -> loop acc (curr + string c) (depth + 1) cs
                    | ')' -> loop acc (curr + string c) (depth - 1) cs
                    | ',' when depth = 0 -> loop (curr :: acc) "" depth cs
                    | _ -> loop acc (curr + string c) depth cs
            loop [] "" 0 (Seq.toList s) |> List.toArray

        let spcMp1 = ((spaceStr.Replace ("\n",""))
                        .Replace("\t","")
                        .Replace(" ",""))
                        |> splitTopLevel
                        |> Array.Parallel.map(fun x -> x.Remove(0,1)) 
                        |> Array.Parallel.map(fun x -> x.Remove(x.Length-1,1))
                        |> Array.Parallel.map (fun x -> x.Split "/")
        let spcMp2 = match ((spcMp1 |> Array.head |> Array.head) = "0") with
                        | true -> spcMp1
                        | false -> Array.append [|[|"0";"Q=22"|]|] spcMp1
        let spcAt1 = spcMp2 
                    |> Array.head 
                    |> Array.tail
                    |> Array.Parallel.map (fun x -> x.Split("="))
                    |> Array.Parallel.map (fun x -> x[0],x[1])
                    |> Map.ofArray

        let spcCt1 = spcMp2 |> Array.tail |> Array.map(fun x -> x[1])
        let spcMp3 = spcMp2 |> Array.tail
        let spcMp4 = Array.map2 (fun x y -> Array.set x 1 y) spcMp3 spcCt1
        let spcMp5 = Array.append [|spcMp2 |> Array.head|] spcMp3
        let spcMp6 = spcMp5 
                    |> Array.tail
                    |> Array.Parallel.map (fun x -> (x[0],(int x[1],x[2]))) 
                    |> Array.sortBy (fun (x,_) -> x)
                    |> Map.ofArray

        let spcKy01 = 
            spcMp6 
            |> Map.keys 
            |> Array.ofSeq 
            |> Array.groupBy(fun x 
                                -> match (x.Length <= 1) with 
                                    |true -> "0"
                                    |false -> x.Substring (0, x.LastIndexOf(".")))
        let spcKy02 = 
            spcKy01 
            |> Array.head 
            |> snd 
            |> Array.windowed 2 
            |> Array.Parallel.map(fun x -> x[0],[|x[1]|])
        let spcKy03 = 
            spcKy01 
            |> Array.tail 
            |> Array.partition (fun (x,_) -> x.Length = 1)
        let spcKy04 = 
            (Array.append spcKy02 (fst spcKy03)) 
            |> Array.groupBy (fun (x,_) -> x)
            |> Array.Parallel.map (fun x -> snd x)
            |> Array.Parallel.map (fun x 
                                    -> (Array.Parallel.map(fun (y,z)
                                                            -> Array.append[|y|] z))x)
            |> Array.Parallel.map (fun x -> Array.concat x)
            |> Array.Parallel.map (fun x -> Array.distinct x)
            |> Array.Parallel.map (fun x -> Array.sort x)
        let spcKy05 = 
            (snd spcKy03)
            |> Array.Parallel.map (fun (x,y) 
                                    -> Array.append [|x|] y)
            |> Array.append spcKy04
            |> Array.sortBy (fun x -> Array.head x)
        let spcKy06 = 
            let a = match (Array.isEmpty spcKy05) with 
                    |  true -> [|[|"1"|]|]
                    | false -> spcKy05
            a
            |> Array.Parallel.map(fun x 
                                        -> (Array.Parallel.map (fun y 
                                                                    -> y, spcMp6 
                                                                    |> Map.find y))x)
        let spcKey =
            spcKy06
            |> Array.Parallel.map (fun z 
                                    -> (Array.Parallel.map (fun (x,y) 
                                                                -> x, fst y, snd y))z)
        spcAt1,spcKey  

    /// <summary> Generate coxels based on string data. </summary>
    /// <param name="seq"> Sequence. </param>
    /// <param name="bas"> Base hexel. </param>
    /// <param name="occ"> Unavailable hexels. </param>
    /// <returns> Coxel array </returns>    
    let spaceCxl
        (occ : Hxl[])
        (str : string) = 
        
        // Attributes
        let spcAt1 = fst (spaceSeq str)
        // Attribute Q for Sequence
        let seq = match spcAt1 |> Map.tryFind "Q" with 
                    | Some a -> match a with 
                                | "VRCWEE" -> VRCWEE
                                | "VRCCEE" -> VRCCEE
                                | "VRCWSE" -> VRCWSE
                                | "VRCCSE" -> VRCCSE
                                | "VRCWSW" -> VRCWSW
                                | "VRCCSW" -> VRCCSW
                                | "VRCWWW" -> VRCWWW
                                | "VRCCWW" -> VRCCWW
                                | "VRCWNW" -> VRCWNW
                                | "VRCCNW" -> VRCCNW
                                | "VRCWNE" -> VRCWNE
                                | "VRCCNE" -> VRCCNE
                                | "HRCWNN" -> HRCWNN
                                | "HRCCNN" -> HRCCNN
                                | "HRCWNE" -> HRCWNE
                                | "HRCCNE" -> HRCCNE
                                | "HRCWSE" -> HRCWSE
                                | "HRCCSE" -> HRCCSE
                                | "HRCWSS" -> HRCWSS
                                | "HRCCSS" -> HRCCSS
                                | "HRCWSW" -> HRCWSW
                                | "HRCCSW" -> HRCCSW
                                | "HRCWNW" -> HRCWNW
                                | "HRCCNW" -> HRCCNW
                                | _        -> VRCWEE
                    | None -> VRCWEE

        // Attribute L for Elevation
        let elv = match spcAt1 |> Map.tryFind "L" with 
                    | Some a -> a |> int
                    | None -> 0

        // Attribute W for Width
        let bdWd = match spcAt1 |> Map.tryFind "W" with 
                    | Some a -> match a |> int > 0 with
                                | true -> a |> int
                                | false -> 0
                    | None -> 0

        // Attribute H for Height
        let bdHt = match spcAt1 |> Map.tryFind "H" with 
                    | Some a -> match bdWd > 0 with 
                                | true -> a |> int
                                | false -> bdWd
                        | None -> bdWd

        let parsePolygonString (s: string) : (int * int)[][] =
            let parseSegment (segment: string) =
                segment.Split(',', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun x -> x.Trim())
                |> Array.choose (fun x ->
                    match System.Int32.TryParse(x) with
                    | true, v -> Some v
                    | false, _ -> None
                )
                |> fun numbers ->
                    match numbers with
                    | [||] -> [||]
                    | ns when ns.Length % 2 <> 0 -> [||]
                    | ns ->
                        ns
                        |> Array.chunkBySize 2
                        |> Array.map (function
                            | [|a; b|] -> a, b
                            | _ -> failwith "Unexpected chunk length"
                        )

            match String.IsNullOrWhiteSpace(s) with
            | true -> [||]
            | false ->
                s.Split('-', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map parseSegment

        // Attribute O for Outer Boundary Vertices
        let bdOu = match spcAt1 |> Map.tryFind "O" with 
                    | Some a -> Array.concat (parsePolygonString a)
                    | None -> match bdWd=0 || bdHt=0 with 
                                | true -> [||]
                                | false -> 
                                        let b = $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0"
                                        Array.concat (parsePolygonString b)

        // Attribute I for Island Boundary Vertices
        let bdIs = match spcAt1 |> Map.tryFind "I" with 
                    | Some a -> parsePolygonString a
                    | None -> [||]

        // Attribute E for Entry Hexel
        let bsHx =
            match spcAt1 |> Map.tryFind "E" with
            | Some a ->
                let parts = a.Split ',' |> Array.choose (fun s ->
                    match System.Int32.TryParse(s.Trim()) with
                    | true, v -> Some v
                    | _ -> None
                )
                match parts with
                | [| x; y |] -> hxlLin seq elv (identity elv) (AV(x, y, elv))
                                |> hxlUni 1
                                |> Array.last
                | _ -> identity elv
            | None -> match bdWd = 0 with
                        | true -> identity elv
                        | false ->  hxlLin seq elv (identity elv) (AV(bdWd/2+2, bdHt/2+2, elv))
                                    |> hxlUni 1
                                    |> Array.last
        
        // Total Count
        let cxlCnt = spaceSeq str 
                    |> snd 
                    |> Array.concat
                    |> Array.Parallel.map (fun (_,x,_) -> x)
                    |> Array.sum |> float

        // Site Net Area
        let ntArea = polygonWithHolesArea bdOu bdIs

        // Attribute X for Count Proportion
        let bdPr = match spcAt1 |> Map.tryFind "X" with 
                    | Some a -> match a with
                                | "0" -> match cxlCnt > 0 with 
                                            | true -> ntArea / cxlCnt
                                            | false -> 1.0
                                | _ -> a |> float
                    | None -> 1.0

        // Outer Hexels 
        let ouHx = match bdWd=0 || bdHt=0 with 
                    | true -> [||]
                    | false -> hxlPgn seq elv bdOu

        let ilHx = match bdWd=0 || bdHt=0 with 
                    | true -> [||]
                    | false -> bdIs |> Array.map (fun x -> hxlPgn seq elv x) |> Array.concat
            
        let occ = Array.concat [|occ;ouHx;ilHx|]

        // Parse Space String
        let tree01 = 
            spaceSeq str
                |> snd
                |> Array.Parallel.map (fun x -> 

                    Array.Parallel.map(fun (a,b,c) 
                                        -> Refid a, Count (int(float b * bdPr)), Label c)x)

        // Generate base coxel
        let id,ct,lb = tree01 |> Array.concat |> Array.head
        let cti  = match ct with 
                    | Count x when x>0 -> Count (x-1) 
                    | _ -> Count 0       
        let ac0 = match cti with 
                    | Count a when a < 1 -> coxel seq elv ([|(identity elv), id, cti, lb|]) occ
                    | _ -> coxel seq elv ([|bsHx, id, cti, lb|]) occ
        let ac1 = [|{ac0[0] with Hxls = Array.except occ (Array.append [|ac0[0].Base|] ac0[0].Hxls)}|]
        let oc1 = (Array.concat [|occ; [|bsHx|]; (Array.head ac1).Hxls|])

        let cxlCxl 
            (seq : Sqn)
            (tre : (Prp*Prp*Prp)[])
            (occ : Hxl[])
            (acc : Cxl[]) = 
            let bsCx = 
                        acc 
                        |> Array.Parallel.map(fun x -> x.Rfid,x) 
                        |> Map.ofArray
                        |> Map.find (tre |> Array.Parallel.map (fun (a,_,_) -> a) |> Array.head)
                            
            // Available Hexels
            let chHx = bsCx.Hxls |> Array.filter (fun x -> (AV(hxlCrd x))=x)
            // Required host Hexel count
            let cnt = (Array.length tre) - 1
            // Seperated host hexels
            let chBs = match (Array.length chHx) >= cnt with 
                        | true -> 
                                    let divs =  ((Array.length chHx) / cnt)
                                    let chnk = Array.chunkBySize divs chHx
                                    let fsHx = chnk |> Array.Parallel.map (fun x -> Array.head x)
                                    Array.take cnt fsHx
                        | false -> Array.append 
                                    chHx 
                                    (Array.replicate (cnt - (Array.length chHx)) (identity elv))
            let chPr = Array.tail tre
            let cxc1 = coxel 
                        seq
                        elv
                        (Array.map2 (fun a (b, c, d) -> a,b,c,d) chBs chPr)
                        occ
            // Reassigning Hexel types
            let chHx1 = Array.Parallel.map (fun x -> x.Hxls) cxc1
            let chOc1 = hxlUni 2 (Array.append occ (Array.concat chHx1))
            let chHx2 = Array.Parallel.map (fun x -> hxlChk seq elv chOc1 x) chHx1
            let chHx3 = hxlChk seq elv chOc1 (Array.map (fun x -> x.Base) cxc1)
            let cxc2 = Array.map3 (fun x y z -> {x with Cxl.Hxls = y; Cxl.Base = z}) cxc1 chHx2 chHx3
            cxc2
            
        let rec cxCxCx
            (seq : Sqn)
            (tre : (Prp*Prp*Prp)[][])
            (occ : Hxl[])
            (acc : Cxl[]) =
                
            let a = match Array.tryHead tre with 
                        | Some a 
                            -> 
                                let occ = Array.append occ (Array.concat (Array.Parallel.map(fun x -> x.Hxls)acc))
                                let tre = Array.tail tre
                                let acc = Array.append 
                                            acc 
                                            (cxlCxl seq a occ acc)
                                cxCxCx seq tre occ acc
                        | None -> acc
            a

        match (Array.length (Array.concat tree01) < 2) with 
        | true -> ac1
        | false -> cxCxCx seq tree01 oc1 ac1

// Test Zone
open Hexel
open Coxel
open Geometry
open Parse
open System
let sq = VRCCEE
// Sample Format
//let spcStr = "(#/W=10/H=10/X=0),(1/7/Foyer),(2/12/Living),(3/8/Dining),(1.1/9/Study),(2.1/12/Staircase),(3.1/14/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.4/18/Bed-3),(3.1.1/6/Utility),(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/10/Closet-3),(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"
//let spcStr1 = "(0/Q=VRCCNE/W=10/L=1),(1/25/Dock),(1.1/25/Logistics),(1.2/25/Lab),(1.3/25/Habitation),(1.4/25/Power)"
//let spcStr1 = "(0/Q=VRCCNE/L=0/W=100/H=100/X=0/O=0,0,0,100,100,100,100,0/I=/E=18,10),(1/24/Dock), (1.1/24/Logistics), (1.2/24/Lab), (1.3/24/Habitation), (1.4/24/Power)"
let spcStr1 = "(0/Q=VRCCNE),(1/24/Dock), (1.1/24/Logistics), (1.2/24/Lab), (1.3/24/Habitation), (1.4/24/Power)"
//let spcStr2 = "(1/5/Start),(2/15/End)"

//let oc1 = hxlPgn sq [|0,0;0,10;10,10;10,0|] 1
let cx1 = spaceCxl [||] spcStr1

Array.last (hxlUni 1 (hxlLin sq 1 (identity 1) (AV(5, 5, 1))))
//let hx1 = cx1[0].Hxls
//let sq11 = VRCWEE

//let ctg = hxlCtg hx1 sq1 

//cx1[1].Hxls |> Array.map(fun x -> x.IsAV)


(* let hx0 =[|AV(0,0,0); AV(-1,2,0); AV(1,2,0); AV(0,4,0); AV(-1,6,0); AV(1,6,0)|]
let hx11 = [|RV (0, 0, 0);RV (2, 0, 0); RV (1, -2, 0); RV (-1, -2, 0); RV (-2, 0, 0);
         RV (-1, 2, 0); RV (1, 2, 0); AV (4, 0, 0); AV (3, 2, 0);
         AV (3, -2, 0); AV (2, -4, 0); AV (0, -4, 0); AV (-2, -4, 0);
         AV (-3, -2, 0); AV (-4, 0, 0); AV (-3, 2, 0); AV (-2, 4, 0);
         AV (0, 4, 0); AV (2, 4, 0); AV (6, 0, 0)|] 
 *)
(* let cxx1 = coxel sq ([| 
            AV (3, -2, 0), Refid "1", Count 10, Label "A"; 
            AV (2, 4, 0), Refid "2", Count 10, Label "B";
            AV (3, -2, 0), Refid "3", Count 10, Label "C";
            |]) hx11 
*)
//let st = hxlVld sq (AV(0,0,0))
//let en = hxlVld sq (AV(20,0,0))

// Create parsing for levels

let pg1 =[|(6,4); (7,6); (5,10); (6,12); (5,14); (6,16); (5,18); (9,26); (7,30); (3,30); (1,26); (2,24); (1,22); (2,20); (1,18); (2,16); (0,12); (1,10); (0,8); (1,6); (0,4); (2,0); (6,0); (7,2)|]

removeSawtooth VRCCEE pg1


