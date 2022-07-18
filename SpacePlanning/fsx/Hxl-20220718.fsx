// Hexel
type Lcn = 
    int * int * double

type Hxl = 
    | Avbl of Lcn
    | Ocpd of Lcn

let hos hst =
    match hst with 
    | Avbl _ -> true
    | Ocpd _ -> false

let xyz hxl = 
    match hxl with 
    | Avbl c -> c
    | Ocpd c -> c

let vld hxl = 
    let xyz = xyz hxl
    let vld = 
        match xyz with 
        | (x,y,z) when (x % 2 = 0) -> (x, y - (y % 4) + 1, z)
        | (x,y,z) -> (x, y - (y%4) + 3, z)
    match hxl with 
    | Avbl _ -> Avbl vld
    | Ocpd _ -> Ocpd vld
    
// Adjacent Hexels
let adj (hst : Hxl) = 
    // Adjacent Hexels
    match hst with 
    | Avbl (x1,y1,z1) -> 
        List.map2 (fun a b -> 
            Avbl ((a+x1), (b+y1), z1)) 
            [0; -2; -1; 1; 2; 1; -1] 
            [0; 0; 2; 2; 0; -2; -2]
    | Ocpd _ -> [hst]

// Update Availability
let chk (hst : Hxl) (occ : Hxl list) = 
    let xyzO = List.map (fun x -> xyz x)occ
    let xyzA  = List.map (fun x -> xyz x) (adj hst)
    let xyzB = List.except xyzO xyzA

    match hst with 
    | Ocpd _ -> hst
    | Avbl (x,y,z) when (List.length xyzB) = 0 -> Ocpd (x,y,z)
    | Avbl _ -> hst

// Incremental Hexel
let inc (hst : Hxl) (occ : Hxl list) = 
    let xyzO1 = List.map (fun x -> xyz x)occ
    let xyzA1  = List.map (fun x -> xyz x) (adj hst)
    let xyzB1 = List.except ((List.head xyzA1)::xyzO1) xyzA1
    
    let in1 = match hst with 
                | Ocpd _ -> adj hst |> List.head
                | Avbl (x,y,z) when (List.length xyzB1) = 0 -> 
                    adj (Ocpd (x,y,z)) |> List.head
                | Avbl _ -> adj (Avbl (xyzB1|> List.head)) |> List.head
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

// Multiple Uniform Increment    
let mlt (hst : Hxl list) (occ : Hxl list) (cnt : int) = 
    let rec inc (hst : Hxl list) (occ : Hxl list) (cnt : int) (acc : Hxl list list)= 
        match cnt with 
        | 1 -> acc
        | cnt -> 
                        let hs1 = List.map(fun a -> List.filter (hos) a) acc
                        let hs2 = List.map2 (fun x y -> match x with 
                                                        | [] -> List.head y
                                                        | x -> List.head x) hs1 acc
                        let hs3 = inr hs2 (hs2@occ@(List.concat acc))
                        let hst = List.map (fun x -> chk x (hs3@occ@(List.concat acc))) hs3
                        let ac1 = List.map(fun x -> List.concat x) (List.transpose([acc;(List.chunkBySize 1 hst)]))
                        let acc = List.map(fun x -> (List.map(fun a -> chk a (occ@(List.concat acc)@hst)))x)ac1
                        inc hst (occ@(List.concat acc)@hst) (cnt-1) acc
    let hs1 = List.map (fun x -> chk x (hst@occ)) hst
    List.map (fun x -> List.distinct x) (inc hs1 occ cnt (List.chunkBySize 1 hs1))

//Testing
let oc1 = (adj (vld (Avbl(0,0,0))))
let hs1 = oc1.[1..5]
let in1 = inr hs1 oc1 
let in2 = mlt hs1 oc1 15