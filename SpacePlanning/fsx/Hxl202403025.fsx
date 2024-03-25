module Hexel =
    /// <summary> Hexel is a location representing an irregular hexagonal module.
    /// Collections of hexels in a hexagonal grid form Coxels.
    /// A hexel can have a maximum of six neighbouring/adjacent hexels.
    /// All neighbouring hexels share at least one common edge </summary>

    /// <summary> Hexel types: Categorization based on location availabity. </summary>
    ///<typeparam name="AV"> AvaiIable Hexels. </typeparam>
    ///<typeparam name="RV"> Reserved Hexels. </typeparam>
    type Hxl = 
        | AV of x:int * y:int * z:int
        | RV of x:int * y:int * z:int

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
    /// <typeparam name="SQ11"> Orientation:Vertical, Flow:Clockwise, Start:East </typeparam>
    /// <typeparam name="SQ12"> Orientation:Vertical, Flow:Anti-Clockwise, Start:East </typeparam>
    /// <typeparam name="SQ13"> Orientation:Vertical, Flow:Clockwise, Start:South-East </typeparam>
    /// <typeparam name="SQ14"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-East </typeparam>
    /// <typeparam name="SQ15"> Orientation:Vertical, Flow:Clockwise, Start:South-West </typeparam>
    /// <typeparam name="SQ16"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-West </typeparam>
    /// <typeparam name="SQ17"> Orientation:Vertical, Flow:Clockwise, Start:West </typeparam>
    /// <typeparam name="SQ18"> Orientation:Vertical, Flow:Anti-Clockwise, Start:West </typeparam>
    /// <typeparam name="SQ19"> Orientation:Vertical, Flow:Clockwise, Start:North-West </typeparam>
    /// <typeparam name="SQ20"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-West </typeparam>
    /// <typeparam name="SQ21"> Orientation:Vertical, Flow:Clockwise, Start:North-East </typeparam>
    /// <typeparam name="SQ22"> Orientation:Vertical, Flow:Anti-Clockwise, Start:North-East </typeparam>
    /// <typeparam name="SQ23"> Orientation:Horizontal, Flow:Clockwise, Start:North </typeparam>
    /// <typeparam name="SQ24"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North </typeparam>
    /// <typeparam name="SQ25"> Orientation:Horizontal, Flow:Clockwise, Start:North-East </typeparam>
    /// <typeparam name="SQ26"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-East </typeparam>
    /// <typeparam name="SQ27"> Orientation:Horizontal, Flow:Clockwise, Start:South-East </typeparam>
    /// <typeparam name="SQ28"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-East </typeparam>
    /// <typeparam name="SQ29"> Orientation:Horizontal, Flow:Clockwise, Start:South </typeparam>
    /// <typeparam name="SQ30"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South </typeparam>
    /// <typeparam name="SQ31"> Orientation:Horizontal, Flow:Clockwise, Start:South-West </typeparam>
    /// <typeparam name="SQ32"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:South-West </typeparam>
    /// <typeparam name="SQ33"> Orientation:Horizontal, Flow:Clockwise, Start:North-West </typeparam>
    /// <typeparam name="SQ34"> Orientation:Horizontal, Flow:Anti-Clockwise, Start:North-West </typeparam>
    type Sqn =  
        | SQ11 | SQ12 | SQ13 | SQ14 | SQ15 | SQ16 | SQ17 | SQ18 | SQ19 | SQ20 | SQ21 | SQ22
        | SQ23 | SQ24 | SQ25 | SQ26 | SQ27 | SQ28 | SQ29 | SQ30 | SQ31 | SQ32 | SQ33 | SQ34

    /// <summary> Sequence Locations: Location of adjacent/neighbouring hexels relative to the host hexel.
    /// Each array begins with the location of Host hexel followed by the rest in a particular order.
    /// Hexadecimal number system - 0x0:0, 0x1:1, 0x2:2, 0xFFFFFFFF:-1, 0xFFFFFFFE:-2 </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <returns> An array of two dimensional surrounding locations. </returns>
    let sequence 
        (sqn:Sqn) =  
        match sqn with 
        | SQ11 -> [|0x0,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2|]
        | SQ12 -> [|0x0,0x0; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE|]
        | SQ13 -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0|]
        | SQ14 -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE|]
        | SQ15 -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE|]
        | SQ16 -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0|]
        | SQ17 -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
        | SQ18 -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2|]
        | SQ19 -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0|]
        | SQ20 -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2|]
        | SQ21 -> [|0x0,0x0; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2|]
        | SQ22 -> [|0x0,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0|]
        | SQ23 -> [|0x0,0x0; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1|]
        | SQ24 -> [|0x0,0x0; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1|]
        | SQ25 -> [|0x0,0x0; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2|]
        | SQ26 -> [|0x0,0x0; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF|]
        | SQ27 -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1|]
        | SQ28 -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
        | SQ29 -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF|]
        | SQ30 -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF|]
        | SQ31 -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
        | SQ32 -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1|]
        | SQ33 -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF|]
        | SQ34 -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2|]
   
    /// <summary> Identity Hexel. </summary>
    /// <returns> Available (AV) Hexel at global origin. </returns>
    let identity = 
        RV(0x0,0x0, 0x0)

    /// <summary> Extract coordinates from hexel. </summary>
    /// <param name="hexel"> Hexel of type AV/RV. </param>
    /// <returns> Tuple of integers representing three dimensional coordinates. </returns>
    let hxlCrd 
        (hxl : Hxl) = 
        match hxl with 
        | AV (a,b,c) -> (a,b,c)
        | RV (a,b,c) -> (a,b,c)

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
                    | SQ11 | SQ12 | SQ13 | SQ14 | SQ15 | SQ16 | SQ17 | SQ18 | SQ19 | SQ20 | SQ21 | SQ22
                        -> match crx,cry with 
                            | a,b when (b%4 = 0) -> (a + (a%2)), b, crz
                            | a,b when (a%2 = 0)-> a+1, (b + (b%2)), crz
                            | _,b-> crx, (b + (b%2)), crz
                    | SQ23 | SQ24 | SQ25 | SQ26 | SQ27 | SQ28 | SQ29 | SQ30 | SQ31 | SQ32 | SQ33 | SQ34
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

    /// <summary> Standardize hexels to type AV </summary>
    /// <param name="rev"> If true, Standardize to type RV. </param>
    /// <param name="hxl"> An array of hexels. </param>
    /// <returns> Converts all hexels to type AV </returns>
    let allAV 
        (rev:bool)
        (hxl:Hxl[]) = 
        hxl
        |> Array.Parallel.map(fun x -> hxlCrd x)
        |> Array.Parallel.map(fun x -> match rev with 
                                                        | true -> RV x
                                                        | false -> AV x)

    /// <summary> Get Hexel from Tuple. </summary>
    let getHxls 
        (hxo : (Hxl*int)[]) = 
        hxo
        |> Array.Parallel.map(fun x 
                                -> fst x)
                        
    /// <summary> Adjacent Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Base hexel. </param> 
    /// <returns> An array of six adjacent hexels. </returns>
    let adjacent 
        (sqn: Sqn)
        (hxo: Hxl) =
        match hxo with 
        | AV (x,y,z) -> Array.Parallel.map 
                            (fun (a,b) -> 
                            AV(x+a, y+b,z))(sequence sqn)
        | RV (x,y,z) -> [|RV(x,y,z)|]

    /// <summary> Increment Hexel. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Tuple containing Base hexel of collection and size. </param> 
    /// <param name="occ"> Occupied/Unavailable hexels. </param>
    /// <returns> Tuple containing the next hexel and size. </returns>
    let increment 
        (sqn : Sqn)
        (hxo : Hxl * int) 
        (occ : Hxl[]) = 
        let occ = Array.concat 
                    [|
                        occ
                        [|(fst hxo)|]
                        [|identity|]
                    |] |> allAV false
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
            | None -> (identity,0xFFFFFFFF)
        | _ -> (identity,0xFFFFFFFF)

    /// <summary> Available Adjacent Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Hexel or Tuple containing Base hexel of collection and size. </param> 
    /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
    /// <returns> The count of unoccupied surrounding hexels. </returns>
    let available 
        (sqn : Sqn)
        (hxo : obj)
        (occ : Hxl[]) =  
        let occ = occ |> allAV false
        let hx1 = match hxo with 
                    | :? (Hxl*int) as (a,_) -> a
                    | :? Hxl as b ->  b
                    | _ -> RV(0,0,0)
        hx1 
        |> adjacent sqn
        |> Array.except 
            (Array.append occ [|hx1|])
        |> Array.length

    ///<summary> Assign Hexel type. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <returns> Reassigned Hexel Types </returns>
    let hxlTyp
        (sqn : Sqn)
        (occ : Hxl[])
        (hxl : Hxl[]) = 
        hxl |> Array.map (fun x -> match (available sqn x (Array.append occ hxl)) < 1 with 
                                    | true -> RV(hxlCrd x)
                                    | false -> AV(hxlCrd x))

    /// <summary> Increment Hexels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxo"> Array of Tuples containing Base hexel of collection and size. </param> 
    /// <param name="occ"> Array of Occupied/Unavailable hexels. </param>
    /// <returns> Array of Tuples containing Base hexel of collection and reduced size. </returns>
    let increments 
        (sqn : Sqn)
        (hxo : (Hxl*int)[]) 
        (occ : Hxl[]) = 
        let occ = (Array.append occ (getHxls hxo)) |> allAV false
        let inc = 
            Array.scan (fun ac st -> 
            let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity|]|]) |> allAV false
            increment sqn st (Array.append[|fst ac|] occ )) 
                hxo[0] hxo
                |> Array.tail
        
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
            let oc1 = Array.concat[|occ;lc1;ic1|] |> allAV false
            let id1 = Array.map(fun y -> Array.findIndex (fun x -> x = y)ic1)ic1
            let bl1 = Array.map2 (fun x y -> x=y) [|(0x0)..(Array.length ic1)-(0x1)|] id1   
            let tp1 = Array.zip3 bl1 ic1 hxo  
            tp1 |> Array.map2 (fun d (a,b,c) 
                                -> match a with 
                                    | true -> b,d
                                    | false -> 
                                            match ((available sqn c oc1) > 0x0) with 
                                            | false -> (fst c),0xFFFFFFFF
                                            | true -> fst(increment sqn c oc1),d) in1
        
        replaceDuplicate sqn hxo inc occ

    /// <summary> Boundary Hexels Ring. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="hxl"> All constituent hexels. </param>
    /// <returns> Boundary/Peripheral hexels. </returns>
    let bndSqn 
        (sqn : Sqn) 
        (hxo : Hxl[]) = 
        /// <summary> Arrange/sort hexels in continuous sequence. </summary>
        /// <param name="sqn"> Sequence to follow. </param>
        /// <param name="hxl"> Array of hexels. </param>
        /// <param name="acc"> Accumulator for recursive function. </param>
        /// <param name="cnt"> Counter. </param>
        /// <returns> Array of sorted hexels </returns>
        let rec arr 
            (sqn : Sqn) 
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
                arr sqn hxl acc (cnt-1) opt

        let hxl = hxo|> Array.sortByDescending 
                    (fun x -> available sqn x hxo)
        let a1 = 
            match hxl with 
            | [||] -> [||]
            | _ -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) true

        let b1 = Array.length a1 = Array.length hxl
        
        let ar1 = match b1 with 
                    | true -> a1
                    | false -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) false
        match hxo with 
        | [||] -> [||]
        | _ ->  match (Array.head hxo) = (AV(hxlCrd (Array.head hxo))) with 
                | true -> ar1
                | false -> allAV true ar1

    /// <summary> Hexel Ring Segment Sequence. </summary>
    let cntSqn
        (sqn : Sqn)
        (hxo : Hxl[]) =      
        let hxl = allAV false hxo
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
                    (fun x -> available sqn x hxl)
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
                | false -> allAV true ar1

module Coxel =
    open Hexel
    /// <summary> Coxels are primarily a collections of unique hexels </summary>
    /// <summary> Coxel type properties </summary>
    /// <typeparam name="Label"> Name </typeparam>
    /// <typeparam name="Refid"> Unique reference ID </typeparam>
    /// <typeparam name="Count"> ANumber of hexels in Coxel </typeparam>
    type Prp = 
        | Label of string
        | Refid of string
        | Count of int

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

    /// <summary> Creating an array of coxels. </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="ini"> An array of tuples containing base hexel, Reference Id, Count/Size, Label. </param>
    /// <param name="occ"> Unavailable hexels. </param>
    /// <returns> An array of coxels. </returns>
    let coxel 
        (sqn : Sqn)
        (ini : (Hxl*Prp*Prp*Prp)[])
        (occ : Hxl[]) = 
        
        let bas = Array.Parallel.map(fun (x,_,y,_) -> x,int(prpVlu y)) ini
        let szn = Array.Parallel.map(fun (_,_,y,z) -> y,z) ini
        let idn = Array.Parallel.map (fun(x,y,_,_)->x,y) ini

        let cnt = 
                bas
                |> Array.Parallel.map (fun x -> snd x)
                |> Array.max
        let acc = Array.chunkBySize 1 bas
        let occ = (Array.append occ (getHxls bas)) |> allAV false 
        
        let rec clsts 
            (hxo: (Hxl*int)[])
            (occ : Hxl[])
            (acc:(Hxl*int)[][])
            (cnt : int) = 
            
            match cnt with 
            | c when c < 0x1 -> acc
            | _ -> 
                    let occ = 
                        acc 
                        |> Array.concat 
                        |> getHxls
                        |> Array.append occ
                        |> Array.append (getHxls hxo)
                        |> Array.append [|identity|]
                        |> Array.distinct
                        |> allAV false

                    let rpt = Array.Parallel.map (fun x 
                                                    -> (snd x) - 0x1) hxo
                    let Hxl =  
                        acc
                        |> Array.Parallel.map (fun x
                                                -> Array.filter (fun a 
                                                                            -> (available sqn a occ) > 0x0) x)
                        |> Array.Parallel.map (fun x 
                                                -> Array.tryHead x)
                        |> Array.Parallel.map (fun x 
                                                 -> match x with
                                                    | Some a -> a 
                                                    | None -> (identity,0xFFFFFFFF))                
                        |> Array.map2 (fun x y 
                                        -> fst y, x) rpt
                    
                    let inc = increments sqn Hxl occ
                            
                    let acc = Array.map2  (fun x y
                                            -> Array.append x y) 
                                acc
                                (Array.chunkBySize 1 inc)

                    let occ = Array.concat[|getHxls (Array.concat [|Array.concat acc; inc;Hxl|]);occ|] |> allAV false

                    (clsts Hxl occ acc (cnt - 0x1))



        let cls = 
            clsts bas occ acc cnt
                |> Array.Parallel.map(fun x 
                                        -> Array.filter(fun (_,z) -> z >= 0) x)
        
        // Avoid single unclustered cell towards the end
        let cl00 = 
            cls
            |> Array.Parallel.map(fun x -> getHxls x)
        let cl01 = 
            cl00
            |> Array.Parallel.map(fun x -> Array.tail x)
            |> Array.Parallel.map(fun x -> Array.filter(fun y -> (available sqn y x) < 5)x)
        let cl1 = Array.map2 (fun x y -> Array.append [|Array.head x|] y) cl00 cl01
        
        let cxl = Array.map3 (fun x y z -> 
                                                let hx1 = z |> hxlTyp sqn (Array.append occ z)
                                                
                                                {
                                                    Name = snd x
                                                    Rfid = snd y
                                                    Size = fst x
                                                    Seqn = sqn
                                                    Base = Array.head hx1
                                                    Hxls = hx1
                                                })szn idn cl1
        cxl

    /// <summary> Categorize constituent Hexels within a Coxel. </summary>
    /// <param name="cxl"> A coxel. </param>
    /// <param name="occ"> Unavailable hexels. </param>
    /// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
    let cxlHxl
        (cxl : Cxl)  = 

        let avrv = cxl.Hxls 
                |> Array.Parallel.partition
                    (fun x -> x = AV(hxlCrd x))
        let rv01 = (snd avrv) 
                |> Array.Parallel.partition
                    (fun x-> (available 
                        cxl.Seqn 
                        (AV(hxlCrd x)) 
                        (allAV false (cxl.Hxls))) < 1)
        let av01 = match (snd rv01) with 
                    | [||] -> avrv |> fst |> bndSqn cxl.Seqn
                    | _ -> avrv |> fst |> cntSqn cxl.Seqn
        let br01 = match (fst rv01) with 
                    | [||] -> rv01 |> snd |> bndSqn cxl.Seqn
                    | _ -> rv01 |> snd |> cntSqn cxl.Seqn
         
        let pr01 = match av01 with 
                        | [||] -> br01
                        | _ -> match br01 with 
                                | [||] -> av01
                                | _ -> match adjacent 
                                        cxl.Seqn 
                                        (Array.last av01) 
                                        |> allAV true
                                        |> Array.contains (Array.head br01) with 
                                        | true -> Array.append av01 br01
                                        | false -> Array.append av01 (Array.rev br01)
        {|
            Base = cxl.Base
            Hxls = cxl.Hxls
            Core = rv01 |> fst 
            Prph = pr01
            Brdr = br01
            Avbl = av01 
        |}    

module Shape = 
    open Hexel
    /// <summary> Ortogonal Hexel Sequence </summary>
    /// <param name="sqn"> Sequence to follow. </param>
    /// <param name="org"> Start Hexel. </param> 
    /// <param name="lgt"> Sequence Length. </param> 
    /// <param name="vrt"> Vertical / Horizontal. </param> 
    /// <returns> Array of Sequential Reserved Hexels. </returns>
    let hxlOrt 
        (sqn : Sqn)
        (org : Hxl)
        (lgt : int)
        (vrt : bool) =
        let hxx,hxy,hxz = org |> hxlVld sqn |> hxlCrd
        let lgt = lgt + (lgt%2)         
        match sqn with
        | SQ11 | SQ12 | SQ13 | SQ14 | SQ15 | SQ16 | SQ17 | SQ18 | SQ19 | SQ20 | SQ21 | SQ22
            -> match vrt with 
                | true -> [|hxy..4..(hxy+lgt+4)|]
                        |> Array.map (fun y -> [|RV(hxx,y,hxz);RV(hxx+1,y+2,hxz)|])
                        |> Array.concat
                        |> Array.take ((lgt/2)+1)
                | false -> Array.map (fun x -> RV (x,hxy,hxz)) [|hxx..2..(hxx+lgt+4)|]
                        |> Array.take ((lgt/2)+1)
        | SQ23 | SQ24 | SQ25 | SQ26 | SQ27 | SQ28 | SQ29 | SQ30 | SQ31 | SQ32 | SQ33 | SQ34
            -> match vrt with
                | true -> Array.map (fun y -> RV (hxx,y,hxz)) [|hxy..2..(hxy+lgt)|]
                        |> Array.take ((lgt/2)+1)
                | false -> [|hxx..4..(hxx+lgt)|] 
                        |> Array.map (fun x -> [|RV(x,hxy,hxz);RV(x+2,hxy+1,hxz)|])
                        |> Array.concat
                        |> Array.take ((lgt/2)+1)

module Parse = 
    open Hexel
    open Coxel
    let spaceSeq 
        (spaceStr:string) = 
        
        let spaceMap = 
            ((spaceStr.Replace ("\n",""))
                .Replace(" ",""))
                .Split ","
                |> Array.map(fun x -> x.Remove(0,1)) 
                |> Array.map(fun x -> x.Remove(x.Length-1,1))
                |> Array.map (fun x -> x.Split "/") 
                |> Array.map (fun x -> (x[0],(int x[1],x[2]))) 
                |> Array.sortBy (fun (x,y) -> x)
                |> Map.ofArray

        let spcKy01 = 
            spaceMap 
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
            |> Array.map(fun x -> x[0],[|x[1]|])
        
        let spcKy03 = 
            spcKy01 
            |> Array.tail 
            |> Array.partition (fun (x,y) -> x.Length = 1)

        let spcKy04 = 
            (Array.append spcKy02 (fst spcKy03)) 
            |> Array.groupBy (fun (x,y) -> x)
            |> Array.map (fun x -> snd x)
            |> Array.map (fun x 
                            -> (Array.map(fun (y,z)
                                            -> Array.append[|y|] z))x)
            |> Array.map (fun x -> Array.concat x)
            |> Array.map (fun x -> Array.distinct x)
            |> Array.map (fun x -> Array.sort x)

        let spcKy05 = 
            (snd spcKy03)
            |> Array.map (fun (x,y) 
                            -> Array.append [|x|] y)
            |> Array.append spcKy04
            |> Array.sortBy (fun x -> Array.head x)
        
        let spcKy06 = 
            spcKy05 
            |> Array.map(fun x 
                            -> (Array.map (fun y 
                                            -> y, spaceMap 
                                            |> Map.find y))x)
        
        let spcKey =
            spcKy06
            |> Array.map (fun z 
                            -> (Array.map (fun (x,y) 
                                            -> x, fst y, snd y))z)
        spcKey    

    let spaceCxl 
        (seq : Sqn)
        (bas : Hxl)
        (occ : Hxl[])
        (str : string) = 
        (*         
        let avlReq 
            (tr01 : (Prp*Prp*Prp)[][]) = 
            let chlMap = 
                tr01
                |> Array.map(fun x -> Array.head x,Array.length x)
                |> Array.map(fun ((a,_,_),b) ->  a, b-1) 
                |> Map.ofArray
            
            let chdCnt = 
                tr01
                |> Array.map(fun x -> 
                    Array.map(fun (a,_,_) 
                                -> Map.tryFind a chlMap)x)
                |> Array.map (fun x 
                                -> Array.map(fun y 
                                                -> match y with 
                                                    | Some y -> y
                                                    | None -> 0)x)
            chdCnt
        *)
        let tree01 = 
            spaceSeq str 
                |> Array.map (fun x -> 
                    Array.map(fun (a,b,c) 
                                -> Refid a, Count b, Label c)x)

        // Generate base coxel
        let id,ct,lb = tree01 |> Array.concat |> Array.head
        let accCxl = coxel seq ([|bas, id, ct, lb|]) occ
        let oc1 = (Array.concat [|occ; [|bas|]; (Array.head accCxl).Hxls|])

        let cxlCxl 
            (seq : Sqn)
            (tre : (Prp*Prp*Prp)[])
            (occ : Hxl[])
            (acc : Cxl[]) = 
            
            let cnt = (Array.length tre) - 1
            let bsId = 
                        acc 
                        |> Array.map(fun x -> x.Rfid,x) 
                        |> Map.ofArray
                        |> Map.find (tre |> Array.map (fun (a,_,_) -> a) |> Array.head)
                        
            let chHx = bsId.Hxls |> Array.filter (fun x -> (AV(hxlCrd x))=x)
            let chBs = Array.take cnt chHx
            let chPr = Array.tail tre
            let cxc1 = coxel 
                        seq
                        (Array.map2 (fun a (b, c, d) -> a,b,c,d) chBs chPr)
                        occ
            // Reassigning Hexel types
            let chHx1 = Array.map (fun x -> x.Hxls) cxc1
            let chOc1 = allAV true (Array.append occ (Array.concat chHx1))
            let chHx2 = Array.map (fun x -> hxlTyp seq chOc1 x) chHx1
            let chHx3 = hxlTyp seq chOc1 (Array.map (fun x -> x.Base) cxc1)
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
                               let occ = Array.append occ (Array.concat (Array.map(fun x -> x.Hxls)acc))
                               let tre = Array.tail tre
                               let acc = Array.append 
                                            acc 
                                            (cxlCxl seq a occ acc)
                               cxCxCx seq tre occ acc
                        | None -> acc
            a

        cxCxCx seq tree01 oc1 accCxl


// Test Zone
open Hexel
open Coxel
open Shape
open Parse
// Sample Format
let spaceStr =
     "(1/5/Foyer),(2/20/Living),(3/20/Dining),
    (4/10/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
    (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
    (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
    (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"
let treeStr = spaceSeq spaceStr
let a = spaceCxl 
            SQ11 
            (AV(1,2,0))
            ((hxlOrt SQ11 (AV(-50,0,0)) 100 false) |> allAV true)
            spaceStr
