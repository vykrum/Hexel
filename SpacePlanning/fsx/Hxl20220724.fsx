// Hexel
type Loc = 
    int * int * double

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
    in1

// Non Uniform Increments
let nui (hsCt : (Hxl * int) list) (occ : Hxl list) = 
    let hst = List.map (fun (x,_) -> x) hsCt
    let cnt = List.map (fun (_,x) -> x) hsCt
    let mx = (List.max cnt) + 1
    let acc = List.map (fun x -> List.singleton x)hst
    let rec inx (hst : Hxl list) (occ : Hxl list) (cnt : int list) (mxc : int) (acc : Hxl list list) = 
        match mxc with 
        | 1 -> acc 
        | mxc -> 
                        let hs1 = List.map2 (fun x y -> match x with 
                                                                               | x when x < 1 -> y
                                                                               | _ -> (inc y occ) )cnt hst
                        let ac1 = List.map(fun x -> List.concat x) (List.transpose [acc;List.map(fun x->[x])hs1])
                        let occ = (occ @ List.concat ac1)
                        let acc = List.map (fun a -> List.map (fun x -> chk x occ)a) ac1
                        let hs1 = List.map(fun a -> List.filter (host) a) acc
                        let hst = List.map (fun x -> List.head x) hs1
                        let cnt = List.map (fun x -> x - 1) cnt
                        inx hst occ cnt (mxc - 1) acc
    List.map (fun x -> List.distinct x) (inx hst occ cnt mx acc) 

//Testing
let ooc = adj (vld (Host(0,0,0)))
let hss = ooc.[1..3]
let hsc = List.zip hss  [5;3;8]
//let ad1 = inc (hss.[4]) ooc
//let in1 = inr hss ooc 
//let in2 = mui hss ooc 8
nui hsc ooc
