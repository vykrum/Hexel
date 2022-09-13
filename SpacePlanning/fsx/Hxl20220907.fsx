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

    // Except Locations
    let exc (exc : Hxl list) (hxl : Hxl list) = 
                  
        let exy hx = List.exists (fun x -> x = xyz hx)
                        (List.map(fun x -> xyz x)exc)
        let hx1 = List.filter (fun x -> (exy x) = false) hxl
        hx1

    let unq (hxl : Hxl list) = 
        let hx1 = hxl |> List.groupBy (fun x -> xyz x)
        let hx2 = hx1 
                |> List.map (fun (x,y) -> 
                    match y with 
                    | y when (y |> List.contains (Nost x)) -> Nost x
                    | _ -> Host x) 
        hx2
    
    // Check Host status
    let chk (hst : Hxl list) (occ : Hxl list) = 
        hst 
            |> List.map (fun a ->  
                match a with 
                | Nost _ -> a
                | Host (x,y,z)
                    when ((List.length (exc (hst @ occ) (adj a))) < 1)
                        -> Nost (x,y,z)
                | Host _ -> a)

    // Incremental Hexels
    let inc (hst : Hxl list) (occ : Hxl list) = 
        
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
            let bl = ([List.head hs2] |> List.contains (List.last hs2) ) && ((List.length hs2)>2)       
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
        chk hs3 (oc1 @ hs3)

    // Non Uniform Cluster
    let nui (hxc : (Hxl*int) list) (occ : Hxl list) = 
        let (hx1,ct1) = hxc|> List.unzip
        let mxc = (List.max ct1) + 1
        let acc = (List.map(fun x -> List.singleton x)hx1)
        let rec nux (hst : Hxl list) (cnt : int list) (occ : Hxl list) (mxc : int) (acc : Hxl list list) = 
            match mxc with 
            | mxc when (mxc <= 1) -> acc
            | _ ->   
                    let h01 = chk hst (occ @ hst @ (List.concat acc))
                    let h02 = (inc hst occ)
                    let h03 = chk h02 (occ @ hst @ h02 @ (List.concat acc))
                    let hss = [h03;h01] |> List.transpose
                    let hs0 = List.zip cnt hss
                    let hs1 = hs0 
                            |> List.map (fun a -> match a with 
                                                    | x,y when x < 1 -> [List.last y]
                                                    | _,y -> [List.head y])        
                    let cnt = List.map (fun x -> x - 1) cnt
                    let oc1 = [occ @ (acc |> List.concat) @ (hs1 |> List.concat) @ h01] |> List.concat 
                    let ac1 = (List.map2(fun x y -> x @ y) acc hs1) 
                    let ac2 = (List.map ( fun y -> chk y oc1) ac1 ) |> List.map (fun x -> unq x)
                            //|> List.map (fun x -> List.distinct x)
                    let ac3 = (List.map(fun a -> List.filter (hck) a) ac2) |> List.map (fun x -> unq x)
                    let hst = (List.map (fun x -> List.head x ) ac3) 
                    let acc = (List.map (fun x -> chk x (oc1 @ hst)) ac1) |> List.map (fun x -> unq x)
                    let occ = [occ @ (acc |> List.concat) @ hst] |> List.concat 
                    nux hst cnt occ (mxc-1) acc

        nux hx1 ct1 occ mxc acc


//Testing
let ooc = adj (vld (Host(0,0,0)))
let hss = ooc.[1..6]
let hsc = List.zip hss  [11;0;0;0;0;0]
let nu1 = nui hsc ooc