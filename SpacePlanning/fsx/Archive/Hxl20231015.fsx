module Hexel =
    /// <summary> 
    /// Hexel is a repeating six sided irregular hexagonal  module
    /// Collections of hexels in a hexagonal grid form Coxels
    /// A hexel can have a maximum of six neighbouring/adjacent hexels
    /// All neighbouring hexels share at least one common edge 
    /// </summary>

    /// <summary> 
    /// Hexel types 
    /// Categorization based on location availabity 
    /// </summary> 
    type Hxl = 
        ///<typeparam name="AV"> AvaiIable Hexels </typeparam>
        ///<typeparam name="RV"> Reserved Hexels </typeparam>
        | AV of x:int * y:int * z:int
        | RV of x:int * y:int * z:int

    /// <summary>
    /// Count of a collection of hexels
    /// </summary>
    type Siz = Siz of cnt:int

    /// <summary>
    /// Initializer : Indicates the base hexel and size of collection
    /// </summary>
    type Hxi = Hxi of hxl:Hxl * cnt:Siz
    
    /// <summary>
    /// Extract coordinates as tuple of integers from Hexel
    /// </summary>
    let hxlVal 
        (hxl : Hxl) = 
        match hxl with 
        | AV (a,b,c) -> (a,b,c)
        | RV (a,b,c) -> (a,b,c)

    /// <summary>
    /// Get (Hexel,Siz) value from Hexel Initializer type
    /// </summary>     
    let hxiVal 
        (hxi:Hxi[]) = 
        Array.map (fun x -> 
                                let (Hxi (hxl,cnt)) = x
                                hxl,cnt) hxi
        
    /// <summary>
    /// Get Integer value from Size type
    /// </summary>   
    let sizVal
        (siz:Siz[]) =
        Array.map (fun x -> 
                                let (Siz i) = x
                                i)siz
  
   /// <summary>
   /// Get Hexel from Initializer
   /// </summary>
    let hxiHxl 
        (hxi : Hxi[]) = 
        hxi
        |> hxiVal 
        |> Array.map(fun x -> fst x)
    
    /// <summary>
    /// Get Size from Initializer
    /// </summary>
    let hxiSiz
        (hxi : Hxi[]) = 
        hxi
        |> hxiVal
        |> Array.map(fun x -> snd x)    
        
    /// <summary> 
    /// Sequence specifies the orientation of hexels,
    /// the direction of flow of adjacent hexels
    /// and the position of the first of the six adjaent hexels 
    /// </summary>
    /// <remarks> 
    /// <para>
    /// Horizontal refers to a Flat Top hexagonal grid
    /// 
    ///  ___ N N ___     ___     ___     ___     ___     ___
    /// /N W\___/N E\___/   \___/   \___/   \___/   \___/   \
    /// \___/   \___/   \___/   \___/   \___/   \___/   \___/
    /// /S W\___/S E\___/   \___/   \___/   \___/   \___/   \
    /// \___/S S\___/   \___/   \___/   \___/   \___/   \___/
    /// 
    /// </para>
    /// <para>
    /// Vertical refers to a Pointy Top hexagonal grid 
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
    type Sqn =   
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
        | SQ11 | SQ12 | SQ13 | SQ14 | SQ15 | SQ16 | SQ17 | SQ18 | SQ19 | SQ20 | SQ21 | SQ22
        | SQ23 | SQ24 | SQ25 | SQ26 | SQ27 | SQ28 | SQ29 | SQ30 | SQ31 | SQ32 | SQ33 | SQ34

    /// <summary>
    /// Sequence Locations: Location of adjacent/neighbouring hexels relative to the host hexel
    /// Each array begins with the location of Host hexel followed by the rest in a particular order
    /// Hexadecimal number system - 0x0:0, 0x1:1, 0x2:2, 0xFFFFFFFF:-1, 0xFFFFFFFE:-2
    /// </summary>
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
    
    /// <summary> 
    /// Identity Hexel: Available (AV) Hexel at global origin 
    /// </summary>
    let identity = 
        AV(0x0,0x0, 0x0)

    /// <summary>
    /// Standardize type by converting all hexels to type AV 
    /// Useful when the type isn't criteria for comparison
    /// </summary>
    let allOG 
        (hxo:Hxl[]) = 
        
        hxo
        |> Array.map(fun x -> hxlVal x)
        |> Array.map(fun x -> AV x)

    // Adjacent Hexels
    let adjacent 
        (sqn: Sqn)
        (hxo: Hxl) = 
        
        match hxo with 
        | AV (x,y,z) -> Array.map 
                            (fun (a,b) -> 
                            AV(x+a, y+b,z))(sequence sqn)
        | RV (x,y,z) -> [|RV(x,y,z)|]

    // Increment Hexel
    let increment 
        (sqn : Sqn)
        (hxi : Hxl*Siz) 
        (occ : Hxl[]) = 
        
        let occ = Array.concat 
                    [|
                        occ
                        [|(fst hxi)|]
                        [|identity|]
                    |] |> allOG
        
        match hxi with 
        | x,y when (sizVal [|y|])[0] >= 0x0 -> 
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
            | None -> (identity,Siz 0xFFFFFFFF)
        | _ -> (identity,Siz 0xFFFFFFFF)

    // Available Adjacent Hexels
    let available 
        (sqn : Sqn)
        (hxo : obj)
        (occ : Hxl[]) = 
        
        let occ = occ |> allOG
        let hx1 = match hxo with 
                    | :? (Hxl*int) as (a,_) -> a
                    | :? Hxl as b ->  b
                    | _ -> identity
        hx1 
        |> adjacent sqn
        |> Array.except 
            (Array.append occ [|hx1|])
        |> Array.length

    // Increment Hexels
    let increments 
        (sqn : Sqn)
        (hxi : Hxi[]) 
        (occ : Hxl[]) = 
        
        let hxo = hxiVal hxi
        let hx1 = Array.map (fun x -> fst x) hxo
        let occ = (Array.append occ hx1) |> allOG
        let inc = 
            Array.scan (fun ac st -> 
            let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity|]|]) |> allOG
            increment sqn st (Array.append[|fst ac|] occ )) 
                hxo[0] hxo
                |> Array.tail
                |> Array.map (fun x -> Hxi x)
        
        let replaceDuplicate 
            (sqn : Sqn)
            (hxo : Hxi[]) 
            (inc : Hxi[]) 
            (occ : Hxl[]) =
            
            let in1 = sizVal (hxiSiz inc)
            let lc1 = hxiHxl hxo 
            let hsz = hxiVal hxo
            let ic1 = hxiHxl inc 
            let oc1 = Array.concat[|occ;lc1;ic1|] |> allOG
            let id1 = Array.map(fun y -> Array.findIndex (fun x -> x = y)ic1)ic1
            let bl1 = Array.map2 (fun x y -> x=y) [|(0x0)..(Array.length ic1)-(0x1)|] id1   
            let tp1 = Array.zip3 bl1 ic1 hsz
            tp1 |> Array.map2 (fun d (a,b,c) 
                                -> match a with 
                                    | true -> b,d
                                    | false -> 
                                            match ((available sqn c oc1) > 0x0) with 
                                            | false -> (fst c),0xFFFFFFFF
                                            | true -> fst(increment sqn c oc1),d) in1
        
        replaceDuplicate sqn hxi inc occ

module Coxel =
    open Hexel
    
    /// Properties
    type Prp = 
        | Label of string
        | Refid of string

    type Cxl = 
        {
            Name : Prp
            Rfid : Prp
            Size : Siz
            Seqn : Sqn
            Base : Hxl
            Hxls : Hxl[]
        }  

    let prpVlu 
        (prp : Prp) = 
        match prp with 
        | Label prp -> prp
        | Refid prp -> prp

    // Coxel
    let coxel 
        (sqn : Sqn)
        (ini : (Hxl*Prp*Siz*Prp)[])
        (occ : Hxl[]) = 
        
        let bas = Array.map(fun (x,_,y,_) -> x,y) ini
        let hxi = Array.map (fun x -> Hxi x) bas
        let szn = Array.map(fun (_,_,y,z) -> y,z) ini
        let idn = Array.map (fun(x,y,_,_)->x,y) ini

        let cnt = 
                bas
                |> Array.map (fun x -> snd x)
                |> Array.max
        let acc = Array.chunkBySize 1 hxi
        let occ = (Array.append 
            occ 
            (Array.map(fun x -> fst x) bas)) 
                |> allOG 
        
        let rec clsts 
            (hxi : Hxi[])
            (occ : Hxl[])
            (acc : Hxi[][])
            (cnt : Siz) = 
            
            match (sizVal [|cnt|])[0] with 
            | c when c < 0x1 -> acc
            | _ -> 
                    let occ = 
                        acc 
                        |> Array.concat 
                        |> hxiHxl
                        |> Array.append occ
                        |> Array.append (hxiHxl hxi)
                        |> Array.append [|identity|]
                        |> Array.distinct
                        |> allOG
                    let rpt = hxi 
                                        |> hxiVal 
                                        |> Array.map (fun (_,x) ->x) 
                                        |> sizVal
                                        |> Array.map (fun x -> x - 0x1) 
                    let hxi =  
                        acc
                        |> Array.map (fun x
                                        -> Array.filter (fun a 
                                                            -> (available sqn a occ) > 0x0) x)
                        |> Array.map (fun x 
                                        -> Array.tryHead x)
                        |> Array.map (fun x 
                                        -> match x with
                                            | Some a -> a 
                                            | None -> Hxi (identity,Siz 0xFFFFFFFF))
                        |> hxiVal                
                        |> Array.map2 (fun x y 
                                        -> fst y, x) rpt
                        |> Array.map (fun (x,y) -> Hxi (x,Siz y))
                    
                    let inc = increments sqn hxi occ 
                            |> Array.map (fun (x,y) -> Hxi (x,Siz y))
                            
                    let acc = Array.map2  (fun x y
                                            -> Array.append x y) 
                                acc
                                (Array.chunkBySize 1 inc)

                    let occ = Array.concat[|hxiHxl (Array.concat [|Array.concat acc; inc;hxi|]);occ|] |> allOG

                    (clsts hxi occ acc (Siz((sizVal [|cnt|])[0] - 0x1)))


        let cls = 
            clsts hxi occ acc cnt
                |> Array.map(fun x 
                                -> Array.filter(fun z -> ((hxiSiz x) |> sizVal |> Array.head)>0) x)

        let cl1 = 
            cls
            |> Array.map(fun x -> hxiHxl x)
        
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

    // Classify Coxel Hexels
    let cxlHxl
        (cxl : Cxl) 
        (occ : Hxl[]) = 

        // Boundry Hexels Ring
        let bndSqn 
            (sqn : Sqn) 
            (hxl : Hxl[]) = 
        
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

        // Hexel Ring Segment Sequence
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

        let cl1 = cxl.Hxls
        // Bounding Hexels
        let cl2 =  Array.tail cl1
        let cl3 = Array.partition(fun x-> (available cxl.Seqn x occ) > 0) cl2
        let bd1 = fst  cl3
        let bd2 = bndSqn cxl.Seqn bd1
        
        // Core Hexels
        let cr1 = snd cl3
        
        let oc1 = Array.concat
                    [|
                        occ 
                        cxl.Hxls
                    |] |> allOG
        
        let cl4 = Array.partition(fun x-> (available cxl.Seqn x oc1) > 0) bd2
        
        // Available Hexels
        let av1= fst cl4
        let av2 = cntSqn cxl.Seqn av1
        
        // Border Hexels
        let br1= snd cl4

        // Output : Base, Hxls, Core, Prph, Brdr, Avbl
        
        {|
            Base = cxl.Base
            Hxls = cl1
            Core = cr1
            Prph = bd2
            Brdr = br1
            Avbl = av2
        |}    


// Test
# time "on"
open Hexel
open Coxel
let og:Hxl = AV(0,0,0)
let sq = SQ11
let t2 = coxel sq [|og,Refid "0", Siz 10 ,Label "A"|] [||]

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
                                            -> Refid a, Siz b, Label c))x)

let a,b,c = treeRef |> Array.concat |> Array.head
let st = coxel sq [|(og , a , b, c)|] [||]
let c01 = (cxlHxl (Array.head st) [||]).Avbl



