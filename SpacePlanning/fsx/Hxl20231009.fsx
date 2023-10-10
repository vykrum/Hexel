module Hexel =
    ///<summary> 
    /// Hexel is a repeating six sided hexagonal (irregular) module
    /// Collections of hexels in a hexagonal grid form Coxels
    /// A hexel can have a maximum of six neighbouring/adjacent hexels
    /// All neighbouring hexels share at least one common edge 
    /// </summary>

    ///<summary> 
    /// Hexel types 
    /// </summary>
    
    type Hxl = 
        ///<typeparam name="AV"> AvaiIable Hexels </typeparam>
        ///<typeparam name="RV"> Reserved Hexels </typeparam>
        | AV of x:int * y:int * z:int
        | RV of x:int * y:int * z:int

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
        ///<typeparam name="VCEE"> Orientation:Vertical, Flow:Clockwise, Start:East </typeparam>
        ///<typeparam name="VAEE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:East </typeparam>
        /// <typeparam name="VCSE"> Orientation:Vertical, Flow:Anti-Clockwise, Start:South-East </typeparam>
        /// <typeparam name="HCNN"> Orientation:Horizontal, Flow:Clockwise, Start:North </typeparam>
        | VCEE | VAEE | VCSE | VASE | VCSW | VASW | VCWW | VAWW | VCNW | VANW | VCNE | VANE
        | HCNN | HANN | HCNE | HANE | HCSE | HASE | HCSS | HASS | HCSW | HASW | HCNW | HANW

    // Sequence Locations
    let sequence 
        (sqn:Sqn) =  
        match sqn with 
        | VCEE -> [|0x0,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2|]
        | VAEE -> [|0x0,0x0; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE|]
        | VCSE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0|]
        | VASE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE|]
        | VCSW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE|]
        | VASW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0|]
        | VCWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
        | VAWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2|]
        | VCNW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0|]
        | VANW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2|]
        | VCNE -> [|0x0,0x0; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2|]
        | VANE -> [|0x0,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0|]
        | HCNN -> [|0x0,0x0; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1|]
        | HANN -> [|0x0,0x0; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1|]
        | HCNE -> [|0x0,0x0; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2|]
        | HANE -> [|0x0,0x0; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF|]
        | HCSE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1|]
        | HASE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
        | HCSS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF|]
        | HASS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF|]
        | HCSW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
        | HASW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1|]
        | HCNW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF|]
        | HANW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2|]
    
    // Identity Hexel
    let identity = 
        AV(0x0,0x0, 0x0)

    // Get Coordinates
    let hxlCrd 
        (hxl : Hxl) = 
        match hxl with 
        | AV (a,b,c) -> (a,b,c)
        | RV (a,b,c) -> (a,b,c)

    // Standardize type
    let allOG 
        (hxo:Hxl[]) = 
        
        hxo
        |> Array.map(fun x -> hxlCrd x)
        |> Array.map(fun x -> AV x)

    // Get Hexel from Tuple
    let getHxls 
        (hxo : (Hxl*int)[]) = 
        
        hxo
        |> Array.map(fun x 
                        -> fst x)
                        
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
        (hxo : Hxl * int) 
        (occ : Hxl[]) = 
        
        let occ = Array.concat 
                    [|
                        occ
                        [|(fst hxo)|]
                        [|identity|]
                    |] |> allOG
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
        (hxo : (Hxl*int)[]) 
        (occ : Hxl[]) = 
        
        let occ = (Array.append occ (getHxls hxo)) |> allOG
        let inc = 
            Array.scan (fun ac st -> 
            let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity|]|]) |> allOG
            increment sqn st (Array.append[|fst ac|] occ )) 
                hxo[0] hxo
                |> Array.tail
        
        let replaceDuplicate 
            (sqn : Sqn)
            (hxo : (Hxl*int)[]) 
            (inc : (Hxl*int)[]) 
            (occ : Hxl[]) =
            
            let in1 = Array.map (fun x -> snd x)inc
            let lc1 = getHxls hxo 
            let ic1 = getHxls inc 
            let oc1 = Array.concat[|occ;lc1;ic1|] |> allOG
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

module Coxel =
    open Hexel
    
    type Prp = 
        | Label of string
        | Refid of string
        | Count of int

    type Cxl = 
        {
            Name : Prp
            Rfid : Prp
            Size : Prp
            Seqn : Sqn
            Base : Hxl
            Hxls : Hxl[]
        }  

    let prpVlu 
        (prp : Prp) = 
        match prp with 
        | Label prp -> prp
        | Refid prp -> prp
        | Count prp -> prp.ToString()

    // Coxel
    let coxel 
        (sqn : Sqn)
        (ini : (Hxl*Prp*Prp*Prp)[])
        (occ : Hxl[]) = 
        
        let bas = Array.map(fun (x,_,y,_) -> x,int(prpVlu y)) ini
        let szn = Array.map(fun (_,_,y,z) -> y,z) ini
        let idn = Array.map (fun(x,y,_,_)->x,y) ini

        let cnt = 
                bas
                |> Array.map (fun x -> snd x)
                |> Array.max
        let acc = Array.chunkBySize 1 bas
        let occ = (Array.append occ (getHxls bas)) |> allOG 
        
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
                        |> allOG

                    let rpt = Array.map (fun x 
                                            -> (snd x) - 0x1) hxo
                    let Hxl =  
                        acc
                        |> Array.map (fun x
                                        -> Array.filter (fun a 
                                                            -> (available sqn a occ) > 0x0) x)
                        |> Array.map (fun x 
                                        -> Array.tryHead x)
                        |> Array.map (fun x 
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

                    let occ = Array.concat[|getHxls (Array.concat [|Array.concat acc; inc;Hxl|]);occ|] |> allOG

                    (clsts Hxl occ acc (cnt - 0x1))


        let cls = 
            clsts bas occ acc cnt
                |> Array.map(fun x 
                                -> Array.filter(fun (_,z) -> z >= 0) x)

        let cl1 = 
            cls
            |> Array.map(fun x -> getHxls x)
        
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
let sq = VCSE
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



