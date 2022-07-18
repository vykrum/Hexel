// Hexel
type Loc = int * int * double

type Hxl = 
    | Host of Loc
    | Nost of Loc

let hos hst =
    match hst with 
    | Host _ -> true
    | Nost _ -> false

let xyz hxl = 
    match hxl with 
    | Host c -> c
    | Nost c -> c

let vld hxl = 
    let xyz = xyz hxl
    let vld = 
        match xyz with 
        | (x,y,z) when (x % 2 = 0) -> (x, y - (y % 4) + 1, z)
        | (x,y,z) -> (x, y - (y%4) + 3, z)
    match hxl with 
    | Host _ -> Host vld
    | Nost _ -> Nost vld
    
// Adjacent Hexels
let adj (hst : Hxl) = 
    // Adjacent Hexels
    match hst with 
    | Host (x1,y1,z1) -> 
        List.map2 (fun a b -> 
            Host ((a+x1), (b+y1), z1)) 
            [0; -2; -1; 1; 2; 1; -1] 
            [0; 0; 2; 2; 0; -2; -2]
    | Nost _ -> [hst]

// Host
let chk (hst : Hxl) (occ : Hxl list) = 
    let xyzO = List.map (fun x -> xyz x)occ
    let xyzA  = List.map (fun x -> xyz x) (adj hst)
    let xyzB = List.except xyzO xyzA

    match hst with 
    | Nost _ -> hst
    | Host (x,y,z) when (List.length xyzB) = 0 -> Nost (x,y,z)
    | Host _ -> hst

// Incremental Hexel
let inc (hst : Hxl) (occ : Hxl list) = 
    let xyzO1 = List.map (fun x -> xyz x)occ
    let xyzA1  = List.map (fun x -> xyz x) (adj hst)
    let xyzB1 = List.except ((List.head xyzA1)::xyzO1) xyzA1
    
    let in1 = match hst with 
                | Nost _ -> adj hst |> List.head
                | Host (x,y,z) when (List.length xyzB1) = 0 -> 
                    adj (Nost (x,y,z)) |> List.head
                | Host _ -> adj (Host (xyzB1|> List.head)) |> List.head
    [chk hst (in1::occ);in1]

// Simultaneous Increment
let inr (hst : Hxl list) (occ : Hxl list) =  
    let hs1 = List.map (fun x -> chk x (hst@occ)) hst
    let rec inc1 (hxl : Hxl list) (occ : Hxl list) (acc : Hxl list) = 
            match hxl with 
            | [] -> acc
            | a :: b -> 
                        let acc = acc @ List.tail(inc a occ)
                        let occ = acc @ occ @ hxl
                        inc1 b occ acc
    inc1 hs1 occ []

// Multiple Increment    
let mlt (hst : Hxl list) (occ : Hxl list) (cnt : int) = 
    let rec inc (hst : Hxl list) (occ : Hxl list) (cnt : int) (acc : Hxl list list)= 
        match cnt with 
        | 1 -> acc
        | cnt -> 
                        let hst = inr hst occ
                        let acc = List.map(fun x -> 
                            List.concat x) (List.transpose([acc;(List.chunkBySize 1 hst)]))
                        let occ = List.concat [occ; List.concat acc]
                        inc hst occ (cnt-1) acc
    let hs1 = List.map (fun x -> chk x (hst@occ)) hst
    inc hs1 occ cnt (List.chunkBySize 1 hs1)

//Testing
let oc1 = (adj (vld (Host(0,0,0))))
let hs1 = oc1.[0..3]
//let ad0 = adj (hs1.[1])
//let ad1 = inc (hs1.[3]) oc1
let in1 = inr hs1 oc1 
let in2 = mlt hs1 oc1 6
(* let a hst occ acc = 
    
    let hs1 = inr hst occ
    let hs2 = List.map(fun a -> List.map (fun x -> chk x (occ @ (List.concat hs1)))a) hs1
    let hs3 = List.map(fun a -> List.filter (host) a) hs2
    let hst = List.map2 (fun x y -> match x with 
                                                    | [] -> List.head y
                                                    | x -> List.head x) hs3 hs2
    let occ = [hst;List.concat acc;oc1] |> List.concat |> List.distinct
    let acc = List.map(fun x -> List.concat x) (List.transpose([acc ; List.map(fun x -> List.tail x) hs2]))
    hs2

a hs1 oc1 (List.chunkBySize 1 hs0) *)