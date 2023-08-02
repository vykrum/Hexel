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
    
    // Hexel xy
    let vxy (x,y,_) = x*10,y*10

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

    // Except Locations
    let exc (exc : Hxl list) (hxl : Hxl list) = 
        let exy hx = List.exists (fun x -> x = xyz hx)
                        (List.map(fun x -> xyz x)exc)
        List.filter (fun x -> (exy x) = false) hxl

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
                            let ac1 = List.map(fun x -> List.concat x) 
                                        (List.transpose [acc;List.map(fun x->[x])hs1])
                            let occ = (occ @ List.concat ac1) |> List.distinct
                            let acc = List.map (fun a -> List.map (fun x -> chk x occ)a) ac1
                            let hs1 = List.map(fun a -> List.filter (hck) a) acc
                            let hs2 = List.map (fun x -> List.head x ) acc
                            let hs3 = List.map2 (fun x y -> x@[y] ) hs1 hs2
                            let hst = List.map (fun x -> List.head x) hs3
                            let cnt = List.map (fun x -> x - 1) cnt
                            inx hst occ cnt (mxc - 1) acc
        let hx1 = List.map (fun x -> List.distinct x) (inx hst occ cnt mx acc) 
        (* let hx2 = (List.map (fun x -> 
                List.except (List.last x) (List.head x))
                    (List.windowed 2 (hx1@ [List.head hx1]))) 
        match (List.length hsCt) with 
        | x when x <= 1 -> hx1
        | _ -> hx2 *)
        hx1

    // Incremental Hexels
    let ins (hst : Hxl list) (occ : Hxl list) = 
        let hs1 = List.map (fun x -> adj x) hst
        let oc1 = occ @ hst
        let hs2 =   hs1 |> List.scan  (fun ac st-> 
                    let oc1 = ac @ oc1
                    let hx1 = (exc oc1 st)
                    let hx2 = hx1 |> List.tryHead
                    let hx3 = match hx2 with 
                                    | Some hx2 -> [hx2]
                                    | None -> [st |> List.head]
                    hx3
                    ) []
                    |> List.tail 
                    |> List.map (fun x -> List.head x)
        // Except hexel in last from first
        let hs3 = 
            let bl = [List.head hs2] |> List.contains (List.last hs2)        
            match bl with 
                | true -> 
                    let hx40 = adj (List.last hst)
                    let hx41 = exc (occ @ hst @ hs2) hx40
                    let hx42 = hx41 |> List.tryHead
                    let hx43 = match hx42 with 
                                                        | Some hx42 -> [hx42]
                                                        | None -> [hst |> List.last]
                    let hx44 = hs2 |> List.rev |> List.tail |> List.rev 
                    let hx45 = hx44 @ hx43
                    hx45
                | false -> hs2
        List.map(fun x -> chk x (occ @ hst @ hs3)) hs3

    // Uniform Cluster
    let uns (hxc : (Hxl*int) list) (occ : Hxl list) = 
        let (hx1,ct1) = hxc |> List.unzip
        let mxc = (List.max ct1) + 1
        let rec nux (hst : Hxl list) (cnt : int list) (occ : Hxl list) (mxc : int) (acc : Hxl list list) = 
            match mxc with 
            | mxc when (mxc <= 1) -> acc
            | _ -> 
                    let hs1 = (ins hst occ) |> List.map(fun x -> List.singleton x)
                    let oc1 = [occ @ (acc |> List.concat) @ (hs1 |> List.concat)] |> List.concat
                    let ac1 = List.map2(fun x y -> x @ y) acc hs1
                    let acc = List.map ( fun y -> List.map (fun x -> chk x oc1)y)ac1
                    let ac2 = List.map(fun a -> List.filter (hck) a) acc
                    let hst = List.map (fun x -> List.head x ) ac2
                    let occ = [occ @ (acc |> List.concat) @ hst] |> List.concat
                    nux hst cnt occ (mxc-1) acc

        nux hx1 ct1 occ mxc (List.map(fun x -> List.singleton x)hx1)


//Testing
let ooc = adj (vld (Host(0,0,0)))
let hss = ooc.[1..6]
let hsc = List.zip hss  [4;5;7;7;7;7]
let nu1 = uns hsc ooc
//let a = ins hss ooc
//let b = ins hss (a @ ooc)
//let c = ins hss (a @ b @ ooc)
