
// Hexel
type Loc = 
    int * int * double

type Hxl = 
    | Host of Loc
    | Nost of Loc

// Host Check
let hck hst =
    match hst with 
    | Host _ -> true
    | Nost _ -> false

// Hexel Locations
let xyz hxl = 
    match hxl with 
    | Host c -> c
    | Nost c -> c

// Valid Locations
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
    
    match hst with 
    | Nost _ -> adj hst |> List.head
    | Host (x,y,z) when (List.length xyzB1) = 0 ->  Nost (x,y,z)
    | Host _ -> adj (Host (xyzB1 |> List.head)) |> List.head
    

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
                        let occ = (occ @ List.concat ac1) |> List.distinct
                        let acc = List.map (fun a -> List.map (fun x -> chk x occ)a) ac1
                        let hs1 = List.map(fun a -> List.filter (hck) a) acc
                        let hs2 = List.map (fun x -> List.head x ) acc
                        let hs3 = List.map2 (fun x y -> x@[y] ) hs1 hs2
                        let hst = List.map (fun x -> List.head x) hs3
                        let cnt = List.map (fun x -> x - 1) cnt
                        inx hst occ cnt (mxc - 1) acc
    let hx1 = List.map (fun x -> List.distinct x) (inx hst occ cnt mx acc) 
    let hx2 = (List.map (fun x -> 
            List.except (List.last x) (List.head x))
                (List.windowed 2 (hx1@ [List.head hx1]))) 
    match (List.length hsCt) with 
    | x when x <= 1 -> hx1
    | _ -> hx2

//  X Y values
let xy (hx ) = 
    let xyz1 = List.map (fun x -> List.map(fun y -> xyz y) x) hx
    let x1 = List.map(fun a->List.map (fun (x,_,_)->x)a)xyz1
    let y1 = List.map(fun a->List.map (fun (_,y,_)->y)a)xyz1
    let xMin = List.min(List.concat x1) + 10
    let x2 = List.map(fun a-> List.map(fun b -> b-xMin)a)x1
    let yMin = List.min(List.concat x1) + 10
    let y2 = List.map(fun c-> List.map(fun d -> d-yMin)c)y1
    [x2;y2]

let xyHx (x,y,_) = x,y
//Testing
let ooc = adj (vld (Host(0,0,0)))

//let hss = ooc.[1..6]
let hss = [ooc.[1]; ooc.[3];ooc.[5]]
let hsc = List.zip hss  [12;20;18]
let nu1 = nui hsc ooc
nu1.[0].[0] |> xyz |> xyHx
//nu1 |> xy
//List.map (fun x -> List.length x) nu1

