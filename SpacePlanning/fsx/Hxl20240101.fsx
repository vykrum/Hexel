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
        AV(0x0,0x0, 0x0)

    /// <summary> Extract coordinates from hexel. </summary>
    /// <param name="hexel"> Hexel of type AV/RV. </param>
    /// <returns> Tuple of integers representing three dimensional coordinates. </returns>
    let hxlCrd 
        (hxl : Hxl) = 
        match hxl with 
        | AV (a,b,c) -> (a,b,c)
        | RV (a,b,c) -> (a,b,c)

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
                    | _ -> identity
        hx1 
        |> adjacent sqn
        |> Array.except 
            (Array.append occ [|hx1|])
        |> Array.length

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
        (hxl : Hxl[]) = 
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
        
        let a1 = 
            match hxl with 
            | [||] -> [||]
            | _ -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) true

        let b1 = Array.length a1 = Array.length hxl
        
        match b1 with 
        | true -> a1
        | false -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) false

    /// <summary> Hexel Ring Segment Sequence. </summary>
    let cntSqn
        (sqn : Sqn)
        (hxl : Hxl[]) =      
        let rec ctSq 
            (sqn : Sqn)
            (hxl : Hxl[])
            (acc : Hxl[])
            (cnt : int) = 
            match cnt with 
            | a when a<=1 -> acc
            | _ -> 
                    let b = Array.last acc
                    let hxl = Array.except [|b|] hxl
                    let d = (adjacent sqn b) |> Array.tail
                    let e = d |> Array.filter(fun x -> Array.contains x hxl) |> Array.tryHead
                    let f = match e with 
                                | Some a -> [|a|]
                                | None -> [||]
                    let acc = Array.append acc f
                    ctSq sqn hxl acc (cnt-1)
        let cnt = Array.length(hxl)
        let arr =  ctSq sqn hxl ([|Array.head hxl|]) cnt
        let bln = cnt = Array.length(arr)
        match bln with 
        | true -> arr
        | false -> ctSq sqn (Array.rev hxl) ([|Array.last hxl|]) cnt

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

        let cl1 = 
            cls
            |> Array.Parallel.map(fun x -> getHxls x)
        
        let cxl = Array.map3 (fun x y z -> 
                                                {
                                                    Name = snd x
                                                    Rfid = snd y
                                                    Size = fst x
                                                    Seqn = sqn
                                                    Base = fst y
                                                    Hxls = z
                                                })szn idn cl1
        cxl

    /// <summary> Categorize constituent Hexels within a Coxel. </summary>
    /// <param name="cxl"> A coxel. </param>
    /// <param name="occ"> Unavailable hexels. </param>
    /// <returns> Hexels categorized as Base, Hxls, Core, Prph, Brdr, Avbl. </returns>
    let cxlHxl
        (cxl : Cxl)     
        (occ : Hxl[]) = 

        /// Bounding Hexels
        let cl1 = cxl.Hxls
        let cl2 =  Array.tail cl1
        let cl3 = Array.Parallel.partition(fun x-> (available cxl.Seqn x occ) > 0) cl2
        let bd1 = fst  cl3
        let bd2 = bndSqn cxl.Seqn bd1
        
        /// Core Hexels
        let cr1 = snd cl3
        
        let oc1 = Array.concat
                    [|
                        occ 
                        cxl.Hxls
                    |] |> allAV false
        
        let cl4 = Array.Parallel.partition(fun x-> (available cxl.Seqn x oc1) > 0) bd2
        
        /// Available Hexels
        let av1= fst cl4
        let av2 = cntSqn cxl.Seqn av1
        
        /// Border Hexels
        let br1= snd cl4
 
        {|
            Base = cxl.Base
            Hxls = cl1
            Core = cr1
            Prph = bd2
            Brdr = br1
            Avbl = av2
        |}    


// Test Zone
# time "on"
open Hexel
open Coxel
let og:Hxl = AV(0,0,0)
let sq = SQ11
let t2 = coxel sq [|og,Refid "0",Count 10,Label "A"|] [||]

#time "off"

let treeStr = 
  [|[|("1", 5, "Foyer"); ("1.1", 10, "Study"); ("2", 20, "Living")|];                             
    [|("2", 20, "Living"); ("3", 20, "Dining")|];
    [|("3", 20, "Dining"); ("3.1", 15, "Bed-1"); ("3.2", 15, "Bed-2"); ("3.3", 15, "Bed-3"); ("3.4", 15, "Kitchen"); ("4", 20, "Staircase")|];                     
    [|("3.1", 15, "Bed-1"); ("3.1.1", 5, "Bath-1")|];                                             
    [|("3.2", 15, "Bed-2"); ("3.2.1", 5, "Dress-2")|];                                            
    [|("3.2.1", 5, "Dress-2"); ("3.2.1.1", 5, "Bath-2")|];                                        
    [|("3.3", 15, "Bed-3"); ("3.3.1", 5, "Dress-3"); ("3.3.2", 5, "Bath-3")|];                    
    [|("3.4", 15, "Kitchen"); ("3.4.1", 5, "Utility")|]|]

let treeRef = treeStr 
            |> Array.map (fun x 
                            ->(Array.map(fun (a,b,c) 
                                            -> Refid a, Count b, Label c))x)

let a,b,c = treeRef |> Array.concat |> Array.head
let st = coxel sq [|(og , a , b, c)|] [||]
let c01 = (cxlHxl (Array.head st) [||]).Avbl

// Rectangle Border Hexels
let wdt = 10
let hgt = 10

