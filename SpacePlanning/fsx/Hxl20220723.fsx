// Hexel
type Loc = int * int * double

type Hxl = 
    | Host of Loc
    | Nost of Loc

let host hst =
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
    | Host (x1,y1,z1) -> List.map2 (fun a b -> Host ((a+x1), (b+y1), z1)) [0; -2; -1; 1; 2; 1; -1] [0; 0; 2; 2; 0; -2; -2]
    | Nost _ -> hst |> List.singleton

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
    let xyzB1 = List.except xyzO1 xyzA1
    
    let in1 = match hst with 
                | Nost _ -> adj hst |> List.head
                | Host (x,y,z) when (List.length xyzB1) = 0 -> adj (Nost (x,y,z)) |> List.head
                | Host _ -> adj (Host (xyzB1 |> List.head)) |> List.head
    [chk hst (in1::occ);in1]

// Simultaneous Increment
let inr (hst : Hxl list) (occ : Hxl list) =  
    List.map (fun x -> 
        let hx1 = inc x occ
        let occ = (inc x occ) @ occ
        hx1 ) hst

let mui (hst : Hxl list) (occ : Hxl list) (cnt : int) = 
    let rec inxx (hst : Hxl list) (occ : Hxl list) (cnt : int) (acc : Hxl list list) = 
        match cnt with 
        | 1 -> acc
        | cnt -> 
                        let hs1 = inr hst occ
                        let hst = List.map (fun x -> List.tail x) hs1
                        let occ = [List.concat acc;List.concat acc;occ] |> List.concat |> List.distinct
                        let ac1 = List.map(fun x -> List.concat x) (List.transpose([acc ; hst]))
                        let acc = List.map(fun a -> List.map (fun x -> chk x occ)a) ac1
                        inxx (List.concat hst) occ (cnt-1) acc
    inxx hst occ cnt (List.chunkBySize 1 hst)

let nui (hst : Hxl list) (occ : Hxl list) (cnt : int list) = 
    let mx = (List.max cnt) + 1
    let acc = List.map (fun x -> List.singleton x)hst
    let rec inx (hst : Hxl list) (occ : Hxl list) (cnt : int list) (mxc : int) (acc : Hxl list list) = 
        match mxc with 
        | 1 -> acc 
        | mxc -> 
                        let hs1 = List.map2 (fun x y -> match x with 
                                                                               | x when x < 1 -> y
                                                                               | _ -> (inc y occ) |> List.last )cnt hst
                        let ac1 = List.map(fun x -> List.concat x) (List.transpose [acc;List.map(fun x->[x])hs1])
                        let occ = (occ @ List.concat ac1)
                        let acc = List.map (fun a -> List.map (fun x -> chk x occ)a) ac1
                        let hst = List.map (fun x -> List.last x) acc
                        let cnt = List.map (fun x -> x - 1) cnt
                        inx hst occ cnt (mxc - 1) acc
    List.map (fun x -> List.distinct x) (inx hst occ cnt mx acc) 
//Testing
let ooc = adj (vld (Host(0,0,0)))
let hss = ooc.[0..3]
//let ad1 = inc (hss.[4]) ooc
//let in1 = inr hss ooc 
//let in2 = mui hss ooc 8
nui hss ooc [0;10;0;0]
