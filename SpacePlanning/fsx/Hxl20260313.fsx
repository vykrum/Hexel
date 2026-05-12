/// <summary> Hexel is a location representing an irregular hexagonal module.
/// Collections of hexels in a hexagonal grid form Coxels.
/// A hexel can have a maximum of six neighbouring/adjacent hexels.
/// All neighbouring hexels share at least one common edge </summary>
module Hexel =
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

    let (|Vertical|Horizontal|) sqn =
        match sqn with
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW 
        | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> Vertical
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE 
        | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW -> Horizontal
    ///

    /// <summary> Sequence Locations: Location of adjacent/neighbouring hexels relative to the host hexel.
    /// Each array begins with the location of Host hexel followed by the rest in a particular order.
    /// Hexadecimal number system - 0x0:0, 0x1:1, 0x2:2, 0xFFFFFFFF:-1, 0xFFFFFFFE:-2 </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <returns> An array of two dimensional surrounding locations. </returns>
    let sequence 
        (sqn:Sqn) =  
        match sqn with 
        | VRCWEE -> [| 0,0;  2, 0;  1,-2; -1,-2; -2, 0; -1, 2;  1, 2|]
        | VRCCEE -> [| 0,0;  2, 0;  1, 2; -1, 2; -2, 0; -1,-2;  1,-2|]
        | VRCWSE -> [| 0,0;  1,-2; -1,-2; -2, 0; -1, 2;  1, 2;  2, 0|]
        | VRCCSE -> [| 0,0;  1,-2;  2, 0;  1, 2; -1, 2; -2, 0; -1,-2|]
        | VRCWSW -> [| 0,0; -1,-2; -2, 0; -1, 2;  1, 2;  2, 0;  1,-2|]
        | VRCCSW -> [| 0,0; -1,-2;  1,-2;  2, 0;  1, 2; -1, 2; -2, 0|]
        | VRCWWW -> [| 0,0; -2, 0; -1, 2;  1, 2;  2, 0;  1,-2; -1,-2|]
        | VRCCWW -> [| 0,0; -2, 0; -1,-2;  1,-2;  2, 0;  1, 2; -1, 2|]
        | VRCWNW -> [| 0,0; -1, 2;  1, 2;  2, 0;  1,-2; -1,-2; -2, 0|]
        | VRCCNW -> [| 0,0; -1, 2; -2, 0; -1,-2;  1,-2;  2, 0;  1, 2|]
        | VRCWNE -> [| 0,0;  1, 2;  2, 0;  1,-2; -1,-2; -2, 0; -1, 2|]
        | VRCCNE -> [| 0,0;  1, 2; -1, 2; -2, 0; -1,-2;  1,-2;  2, 0|]
        | HRCWNN -> [| 0,0;  0, 2;  2, 1;  2,-1;  0,-2; -2,-1; -2, 1|]
        | HRCCNN -> [| 0,0;  0, 2; -2, 1; -2,-1;  0,-2;  2,-1;  2, 1|]
        | HRCWNE -> [| 0,0;  2, 1;  2,-1;  0,-2; -2,-1; -2, 1;  0, 2|]
        | HRCCNE -> [| 0,0;  2, 1;  0, 2; -2, 1; -2,-1;  0,-2;  2,-1|]
        | HRCWSE -> [| 0,0;  2,-1;  0,-2; -2,-1; -2, 1;  0, 2;  2, 1|]
        | HRCCSE -> [| 0,0;  2,-1;  2, 1;  0, 2; -2, 1; -2,-1;  0,-2|]
        | HRCWSS -> [| 0,0;  0,-2; -2,-1; -2, 1;  0, 2;  2, 1;  2,-1|]
        | HRCCSS -> [| 0,0;  0,-2;  2,-1;  2, 1;  0, 2; -2, 1; -2,-1|]
        | HRCWSW -> [| 0,0; -2,-1; -2, 1;  0, 2;  2, 1;  2,-1;  0,-2|]
        | HRCCSW -> [| 0,0; -2,-1;  0,-2;  2,-1;  2, 1;  0, 2; -2, 1|]
        | HRCWNW -> [| 0,0; -2, 1;  0, 2;  2, 1;  2,-1;  0,-2; -2,-1|]
        | HRCCNW -> [| 0,0; -2, 1; -2,-1;  0,-2;  2,-1;  2, 1;  0, 2|]
    ///

    let sqnArray = [|
        VRCWEE; VRCCEE; VRCWSE; VRCCSE; VRCWSW; VRCCSW; VRCWWW; VRCCWW; VRCWNW; VRCCNW; VRCWNE; VRCCNE
        HRCWNN; HRCCNN; HRCWNE; HRCCNE; HRCWSE; HRCCSE; HRCWSS; HRCCSS; HRCWSW; HRCCSW; HRCWNW; HRCCNW
    |]
    ///

    /// <summary> Identity Hexel. </summary>
    /// <returns> Available (AV) Hexel at global origin. </returns>
    let identity 
        (elv:int) = 
        RV(0,0, elv)
    ///

    /// <summary> Extract coordinates from hexel. </summary>
    /// <param name="hexel"> Hexel of type AV/RV. </param>
    /// <returns> Tuple of integers representing three dimensional coordinates. </returns>
    let hxlCrd = function
        | AV(x,y,z) | RV(x,y,z) | EX(x,y,z) -> (x,y,z)
    ///

    /// <summary> Valid Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> Hexel whose coordinates need to be validated. </param> 
    /// <returns> Valid hexel coordinates. </returns>
    let hxlVld 
        (sqn : Sqn)
        (hxl : Hxl) = 
        let validate s x y z =
            match s, x, y with
            // Using our Active Pattern 'Vertical'
            | Vertical, a, b when b % 4 = 0 -> (a + (a % 2)), b, z
            | Vertical, a, b when a % 2 = 0 -> a + 1, (b + (b % 2)), z
            | Vertical, _, b                -> x, (b + (b % 2)), z
            
            // Using our Active Pattern 'Horizontal'
            | Horizontal, a, b when a % 4 = 0 -> a, (b + (b % 2)), z
            | Horizontal, a, b when b % 2 = 0 -> (a + (a % 2)), b + 1, z
            | Horizontal, a, _                -> (a + (a % 2)), y, z

        let x, y, z = hxlCrd hxl
        let vld = validate sqn x y z |> fun (x1, y1, z1) -> validate sqn x1 y1 z1

        match hxl with
        | AV _ -> AV vld
        | RV _ -> RV vld
        | EX _ -> EX vld
    ///

    /// <summary> Change all hexel types to a uniform type.</summary>
    /// <param name="opt"> 1:AV, 2:RV, 3:EX. </param>
    /// <param name="hxl"> An array of hexels. </param>
    /// <returns> Converts all opted type </returns>
    let hxlUni
        (opt : int)
        (hxl : Hxl[]) = 
        let constructor = 
                match opt with
                | 1 -> AV
                | 2 -> RV
                | 3 -> EX
                | _ -> AV
        hxl |> Array.map (hxlCrd >> constructor)
    ///

    /// <summary> Get Hexel from Tuple. </summary>
    /// <param name="hxo"> Tuple containing Base hexel of collection and size. </param>
    let getHxls 
        (hxo : (Hxl*int)[]) = 
        hxo |> Array.map fst
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
        | x,y when y >= 0 -> 
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
            | None -> (hxlVld sqn (identity elv),-1)
        | _ -> (hxlVld sqn (identity elv),-1)
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
                                            | false -> (fst c),-1
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
            | a when cnt <= 1 -> acc
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
    let prpVlu = function 
        | Label s | Refid s -> s
        | Count i -> string i
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
            
        let bas = ini |> Array.map (fun (h, _, p, _) -> h, int (prpVlu p))
        let szn = ini |> Array.map (fun (_, _, y, z) -> y, z)
        let idn = ini |> Array.map (fun (h, r, _, _) -> h, r)

        let cnt = bas |> Array.map snd |> function [||] -> 0 | x -> Array.max x
        let acc = bas |> Array.map (fun x -> [| x |])
        let oc1 = (Array.append occ (getHxls bas)) |> hxlUni 1
            
        let rec clsts 
            (hxo : (Hxl * int)[]) 
            (elv : int) 
            (occ : Hxl[]) 
            (acc : (Hxl * int)[][]) 
            (cnt : int) = 
        
            match cnt with 
            | c when c < 1 -> acc
            | _ -> 
                //Fuse transformation: Find new heads and decrement counts in one pass
                let hx1 = 
                    acc |> Array.Parallel.mapi (fun i row ->
                        let (_, count) = hxo.[i]
                        row 
                        |> Array.tryFind (fun a -> (available sqn elv a occ) > 0)
                        |> function
                        | Some (h, _) -> (h, count - 1)
                        | None        -> (hxlVld sqn (RV(0,0,elv)), 0xFFFFFFFF)
                    )

                let inc = increments sqn elv hx1 occ
                                
                //Efficiently grow the accumulator
                let nextAcc = 
                    Array.map2 (fun current (newEl: Hxl * int) -> 
                        Array.append current [| newEl |]) 
                        acc 
                        inc

                //Rebuild occupancy: Extract Hxl from (Hxl * int) tuples explicitly
                let nextOcc = 
                    [| 
                        yield! occ
                        yield! (hxo |> Array.map fst) // Extract Hxl from hxo
                        yield! (inc |> Array.map fst) // Extract Hxl from inc
                        yield! (hx1 |> Array.map fst) // Extract Hxl from hx1
                    |] 
                    |> Array.distinct
                    |> hxlUni 1

                clsts hx1 elv nextOcc nextAcc (cnt - 1)

        let cls = 
            clsts bas elv oc1 acc cnt
            |> Array.Parallel.map (fun row -> 
                row |> Array.filter (fun (_, z) -> z >= 0))
            
        let cl1 = cls |> Array.Parallel.map getHxls

        Array.map3 (fun (y, z) (h, r) (cluster: Hxl[]) ->
                let hx1 = hxlChk sqn elv (Array.append occ cluster) cluster

                match hx1 with
                | [||] ->
                    {
                        Name = z; Rfid = r; Size = y; Seqn = sqn
                        Base = identity elv; Hxls = [||]
                    }
                | _ ->
                    let head = hx1.[0]
                    let rest = hx1 |> Array.filter (fun x -> x <> head && x <> identity elv)
                    {
                        Name = z; Rfid = r; Size = y; Seqn = sqn
                        Base = head; Hxls = rest
                    }
            ) szn idn cl1
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
        let pr02 = 
            match pr01 with
            // If array has 0, 1, or 2 elements, just return it
            | [| |] | [| _ |] | [| _; _ |] -> pr01 
            | _ ->
                let (x1, y1, _) = hxlCrd (Array.last pr01)
                let (x2, y2, _) = hxlCrd (Array.head pr01)
                let (x3, y3, _) = hxlCrd pr01.[1]
            
                // The cross product (signed area)
                let gs = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

                // Compare sign to determine orientation
                match sign gs with
                | -1 -> pr01                // Already correct orientation
                | 0  ->                     // Collinear case
                    match x2 > x1 with 
                    | true -> pr01 
                    | false -> Array.rev pr01
                | _  -> Array.rev pr01      // Opposite orientation, flip it

        {|
            Base = cxl.Base
            Hxls = cxl.Hxls
            Core = rv01 |> fst 
            Prph = pr02 
            Brdr = br01
            Avbl = av01 
        |}  
    ///

    /// <summary> Coxel Offseted Boundary Wrap </summary>
    /// <param name="cxl"> Coxel. </param>
    /// <returns> Boundary Wrap vertices. </returns>
    let (|Collinear|Turning|) (p1, p2, p3) =
        let (x1, y1), (x2, y2), (x3, y3) = p1, p2, p3
        let crossProduct = (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
        match crossProduct with
        | 0 -> Collinear
        | _ -> Turning

    let cxlPrm 
        (cxl : Cxl) 
        (elv : int) =
        let rec clean points =
            match points with
            // Match 3 points at a time and apply our Active Pattern
            | p1 :: p2 :: p3 :: rest ->
                match (p1, p2, p3) with
                | Collinear -> clean (p1 :: p3 :: rest)      // Drop p2
                | Turning   -> p1 :: clean (p2 :: p3 :: rest) // Keep p1, move to p2
            | _ -> points

        hxlOfs cxl.Seqn elv cxl.Hxls 
        |> Array.map (hxlCrd >> (fun (x, y, _) -> x, y))
        |> Array.toList
        |> clean
        |> List.toArray
    ///

    /// <summary> Coxel Center </summary>
    /// <param name="cxl"> Coxel. </param>
    /// <returns> Coxel hexel closest to center </returns> 
    let cxlCnt 
        (cxl : Cxl): int * int = 
        match cxl.Hxls with
        | [||] -> 
            (-10, -10) 
        | hxls ->
            let hxXY : (int * int)[] = 
                hxls 
                |> Array.map (fun a -> 
                    let x, y, _ = hxlCrd a
                    x, y
                )
            let numPoints = hxXY.Length
            let sumX = hxXY |> Array.sumBy fst
            let sumY = hxXY |> Array.sumBy snd
            
            let centerX = sumX / numPoints
            let centerY = sumY / numPoints
            let center = (centerX, centerY)
            let closestHxl = 
                        hxls 
                        |> Array.minBy (fun hxl ->
                            let x, y, _ = hxlCrd hxl
                            let centerX, centerY = center 
                            let dx = x - centerX
                            let dy = y - centerY
                            dx*dx + dy*dy
                        )
            let finalX, finalY, _ = hxlCrd closestHxl
            (finalX, finalY)

module Geometry =
    open Hexel
    ///

    /// <summary> Hexel Line </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="stt"> Start Hexel. </param> 
    /// <param name="enn"> End Hexel. </param> 
    /// <returns> Array of Sequential Reserved Hexels. </returns>
    let hxlLin 
        (sqn : Sqn) 
        (elv : int) 
        (stt : Hxl) 
        (enn : Hxl) =
        let safeHxlCrd (hOpt: Hxl option) =
            match hOpt with
            | Some h -> hxlCrd h
            | None -> (0, 0, elv)
        let sx, sy, sz = safeHxlCrd (Some stt)
        let ex, ey, _ = safeHxlCrd (Some enn)

        let splitOddChunks (n: int) (arr: 'T[]) : 'T[][] =
            let len = arr.Length
            let rec loop i start acc =
                match i >= n with
                | true -> acc |> List.rev |> Array.ofList
                | false ->
                    let remaining = len - start
                    let remainingChunks = n - i
                    let fairSize = remaining / remainingChunks
                    let size =
                        match i = n - 1 with
                        | true -> remaining
                        | false ->
                            match fairSize % 2, remaining > remainingChunks with
                            | 0, true -> fairSize + 1
                            | _ -> fairSize
                    loop (i + 1) (start + size) (arr.[start .. start + size - 1] :: acc)
            loop 0 0 []

        let dropAlternate (arr) =
            arr |> Array.mapi (fun i x -> i, x)
                |> Array.choose (fun (i, x) -> match i % 2 with 0 -> Some x | _ -> None)

        let bumpEveryOther (hr: bool) (arr: (int * int)[]) =
            arr |> Array.mapi (fun i (x, y) ->
                match i % 2, hr with
                | 0, _ -> (x, y)
                | _, true -> (x, y + 1)
                | _, false -> (x + 1, y))

        let seqH = match sqn with Horizontal -> true | Vertical -> false
        let pty, flt =  
            let step p1 p2 d = match p1 <= p2 with true -> [|p1 .. d .. p2|] | false -> [|p1 .. -d .. p2|]
            match seqH with
            | true -> step sx ex 2, step sy ey 1
            | false -> step sy ey 2, step sx ex 1
                
        let div = 
            match Array.length pty >= Array.length flt with  
            | true -> 
                let cnk1 = splitOddChunks (Array.length flt) pty
                Array.mapi (fun i a -> 
                    match seqH with
                    | true -> cnk1.[i] |> Array.map (fun b -> (b, a))
                    | false -> cnk1.[i] |> Array.map (fun b -> (a, b))
                ) flt |> Array.map (bumpEveryOther seqH) |> Array.concat
            | false -> 
                let cnk1 = splitOddChunks (Array.length pty) flt
                Array.mapi (fun i a -> 
                    match seqH with
                    | true -> cnk1.[i] |> Array.map (fun b -> (a, b))
                    | false -> cnk1.[i] |> Array.map (fun b -> (b, a))
                ) pty |> Array.map dropAlternate |> Array.concat

        let result = div |> Array.map (fun (a,b) -> RV(a, b, sz))
        match result.Length with 0 -> [| stt; enn |] |> Array.distinct | _ -> result
    ///

    let filterOddSecondary sqn (arr: (int * int)[]) =
            let secondary =
                match sqn with
                | Vertical   -> snd
                | Horizontal -> fst

            let parityCounts =
                arr
                |> Array.countBy (fun p -> secondary p % 2)

            let targetParity =
                parityCounts |> Array.maxBy snd |> fst

            arr |> Array.filter (fun p -> secondary p % 2 = targetParity)
    /// 

    /// <summary> Calculates the signed area of a polygon using the Shoelace formula. </summary>
    /// <param name="poly"> Array of (x, y) coordinates defining the polygon. </param>
    /// <returns> The calculated area as a float. </returns>
    let polygonArea 
        (poly: (int * int)[]) =
        match poly with
        | [||] | [|_|] | [|_; _|] -> 0.0
        | pts ->
            let n = pts.Length
            let area = 
                pts 
                |> Array.mapi (fun i (x, y) ->
                    let (nx, ny) = pts.[(i + 1) % n]
                    (x * ny) - (nx * y))
                |> Array.sum
            float area / 2.0

    ///

    /// <summary> Calculates the net area of a polygon by subtracting the area of holes. </summary>
    /// <param name="outer"> Coordinates of the outer boundary. </param>
    /// <param name="holes"> Array of coordinate arrays defining interior holes. </param>
    /// <returns> The net area as a float. </returns>
    let polygonWithHolesArea 
        (outer: (int * int)[]) 
        (holes: (int * int)[][]) =

        match outer, holes with
        | [||], _ -> 0.0
        | _, [||] -> abs (polygonArea outer)
        | outerPts, holePolys ->
            let outerArea = abs (polygonArea outerPts)
            let holesArea =
                holePolys
                |> Array.sumBy (fun hole ->
                    match hole with
                    | [||] | [| _ |] | [| _; _ |] -> 0.0
                    | _ -> abs (polygonArea hole))
            outerArea - holesArea

    /// <summary> Ensures that a polygon vertex array is closed by appending the first vertex to the end if necessary. </summary>
    let ensureClosed (pts: (int*int)[]) =
        match pts with
        | [||] -> pts
        | _ ->
            let first = pts.[0]
            let last = pts.[pts.Length-1]
            if first = last then pts
            else Array.append pts [| first |]

    /// <summary> Removes consecutive duplicate points from a polygon vertex array. </summary>
    let dedupeSequential (pts: (int*int)[]) =
        pts
        |> Array.fold (fun acc p ->
            match acc with
            | [] -> [p]
            | h::_ when h = p -> acc
            | _ -> p :: acc
        ) []
        |> List.rev
        |> Array.ofList

    /// <summary> Normalizes the winding order of a polygon to be either clockwise or counterclockwise. </summary>
    let normalizeWinding (ccw: bool) (pts: (int*int)[]) =
        let pts = ensureClosed pts |> dedupeSequential
        if pts.Length < 4 then pts
        else
            let a = polygonArea pts
            match ccw, a > 0.0 with
            | true, false -> Array.rev pts
            | false, true -> Array.rev pts
            | _ -> pts

    /// <summary> Computes the bounding box of a set of points. </summary>
    let bounds (pts: (int*int)[]) =
        let xs = pts |> Array.map fst
        let ys = pts |> Array.map snd
        Array.min xs, Array.min ys, Array.max xs, Array.max ys

    /// <summary> Computes the centroid (geometric center) of a set of points. </summary>
    let centroid (pts: (int*int)[]) =
        let n = float pts.Length
        let sx = pts |> Array.sumBy (fst >> float)
        let sy = pts |> Array.sumBy (snd >> float)
        sx / n, sy / n

    /// <summary> Determines if a point is inside a polygon using the ray-casting algorithm. </summary>
    let pointInPolygon (px,py) (poly:(int*int)[]) =
        let rec loop i j inside =
            if i = poly.Length then inside else
            let xi, yi = poly.[i]
            let xj, yj = poly.[j]
            let intersect =
                ((yi > py) <> (yj > py)) &&
                (px < (xj-xi) * (py-yi) / (yj-yi+1) + xi)
            loop (i+1) i (if intersect then not inside else inside)
        loop 0 (poly.Length-1) false

    /// <summary> Determines if two line segments intersect. </summary>
    let ccw (ax,ay) (bx,by) (cx,cy) =
        (cy-ay)*(bx-ax) > (by-ay)*(cx-ax)

    /// <summary> Checks if the line segments AB and CD intersect. </summary>
    let segmentsIntersect a b c d =
        ccw a c d <> ccw b c d && ccw a b c <> ccw a b d

    /// <summary> Determines if a polygon has self-intersecting edges. </summary>
    let hasSelfIntersections (pts:(int*int)[]) =
        let p = ensureClosed pts
        let n = p.Length-1
        seq {
            for i in 0..n-2 do
                for j in i+2..n-2 do
                    if i <> 0 || j <> n-2 then
                        yield segmentsIntersect p.[i] p.[i+1] p.[j] p.[j+1]
        } |> Seq.exists id

    /// <summary> Determines if three points are collinear. </summary>
    let isCollinear (ax,ay) (bx,by) (cx,cy) =
        (bx-ax)*(cy-ay) = (by-ay)*(cx-ax)

    let removeCollinear (pts:(int*int)[]) =
        let p = ensureClosed pts
        [|
            for i in 1..p.Length-2 do
                let a = p.[i-1]
                let b = p.[i]
                let c = p.[i+1]
                if not (isCollinear a b c) then yield b
        |]
        |> fun mid -> Array.concat [| [|p.[0]|]; mid; [|p.[p.Length-1]|] |]

    /// <summary> Cleans a polygon by deduplicating vertices, ensuring closure, removing collinear points, and eliminating sawtooth artifacts. </summary>
    let cleanPolygon sqn pts =
        pts
        |> dedupeSequential
        |> ensureClosed
        |> removeCollinear
        |> filterOddSecondary sqn
        |> normalizeWinding true
    ///

    /// <summary> Hexel Polygon </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="vtx"> Integer coordinates of polygon vertices. </param> 
    /// <param name="elv"> Elevation/Level/Z. </param> 
    /// <returns> Array of Sequential Reserved Hexels. </returns>
    let hxlPgn 
        (sqn: Sqn) 
        (elv: int)
        (vtx: (int * int)[]): Hxl[] =

        if vtx.Length < 2 then [||] else
        let stx, sty = Array.head vtx
        let xx, yy, _ =
            hxlLin sqn elv (RV(0,0,elv)) (RV(stx,sty,elv))
            |> Array.last
            |> hxlCrd

        // Replace start vertex with endpoint of (0,0) first vertex
        let vt1 = Array.concat [| [|xx,yy|]; Array.tail vtx |]

        // Ensure closure
        let verts = cleanPolygon sqn vt1

        // Build polygon edges, chaining each segment
        let acc = ResizeArray<Hxl>()
        let mutable lastOpt : Hxl option = None

        for (x2, y2) in verts do
            match lastOpt with
            | None ->
                lastOpt <- Some (RV(x2, y2, elv))
            | Some last ->
                let seg = hxlLin sqn elv last (RV(x2, y2, elv))
                acc.AddRange(seg)
                lastOpt <- Some (Array.last seg)

        acc.ToArray()
        
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

//Array.last (hxlUni 1 (hxlLin sq 1 (identity 1) (AV(5, 5, 1))))
//let hx1 = cx1[0].Hxls
//let sq11 = VRCWEE

//let ctg = hxlCtg hx1 sq1 

//cx1[1].Hxls |> Array.map(fun x -> x.IsAV)


let hx0 =[|AV(0,0,0); AV(-1,2,0); AV(1,2,0); AV(0,4,0); AV(-1,6,0); AV(1,6,0)|]
let hx11 = [|RV (0, 0, 0);RV (2, 0, 0); RV (1, -2, 0); RV (-1, -2, 0); RV (-2, 0, 0);
         RV (-1, 2, 0); RV (1, 2, 0); AV (4, 0, 0); AV (3, 2, 0);
         AV (3, -2, 0); AV (2, -4, 0); AV (0, -4, 0); AV (-2, -4, 0);
         AV (-3, -2, 0); AV (-4, 0, 0); AV (-3, 2, 0); AV (-2, 4, 0);
         AV (0, 4, 0); AV (2, 4, 0); AV (6, 0, 0)|] 

let cxx1 = coxel sq 0 ([| 
            AV (3, -2, 0), Refid "1", Count 10, Label "A"; 
            AV (2, 4, 0), Refid "2", Count 10, Label "B";
            AV (3, -2, 0), Refid "3", Count 10, Label "C";
            |]) hx11 

//let st = hxlVld sq (AV(0,0,0))
//let en = hxlVld sq (AV(20,0,0))

// Create parsing for levels


