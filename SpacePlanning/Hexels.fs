namespace Hexel

// Coordinates
type xyz = { X : int
             Y : int
             Z : double }

// Available, Boundary, Coordinates
type hxl = { A : bool
             B : bool
             C : xyz }

module Cluster = 
    // Valid Hexel Coordinates
    let xyzVldXyz (xyz : xyz) : xyz = 
        match (xyz.X % 2 = 0) with 
        | true -> { xyz with Y = xyz.Y - (xyz.Y % 4) + 1 }
        | false -> { xyz with Y = xyz.Y - (xyz.Y % 4) + 3 }

    // Adjacent Hexels
    let xyzAdjXyz (hxl : hxl) : xyz list = 
        let vldHxl = {hxl with C = xyzVldXyz (hxl.C)}
        match vldHxl.A with 
        | true -> List.map2 (fun x y -> { 
                                            X = hxl.C.X + x
                                            Y = hxl.C.Y + y
                                            Z = hxl.C.Z 
                                        }
                            )
                            [-2;-1;1;2;1;-1] [0;2;2;0;-2;-2]
        | false -> []

    // Host Availability
    let hxlHstAvl (hxl : hxl) (occ : hxl list) : hxl = 
        let xyzOc1 = List.map (fun x -> x.C) occ
        let lgt =  List.except (hxl.C :: xyzAdjXyz hxl) xyzOc1
                    |> List.length
        match (lgt = 0) with 
        | true -> { hxl with A = false }
        | false -> hxl

    // Duplicate check
    let hxlAvlChk (hxl : hxl list) =
        hxl 
        |> List.groupBy (fun x -> x.C) 
        |> List.map (fun(_,x) -> List.distinct x) 
        |> List.map (fun x -> match (List.length x > 1) with
                                    | true -> List.map (fun a -> { (a:hxl) with A = false })x 
                                            |> List.distinct
                                    | false -> x )
        
    // Unique Hexels
    let hxlUnqAvl (hxl : hxl list) : hxl list  = 
        let hxl1 = List.groupBy(fun x -> x.C) hxl
        let xyz1 = List.map (fun ( x, _ ) -> x) hxl1
        let avl1 = List.map (fun ( _, x ) -> x) hxl1
                |> List.map (fun x -> List.map (fun x -> x.A)x)
                |> List.map (fun x -> List.contains false x)
                |> List.map (fun x -> x = false)
        List.map2 (fun x y -> {A = y ; B = false ; C = x}) xyz1 avl1
        
    // Incremental Hexel
    let hxlIncHx1 (hxl : hxl) (occ : hxl list) : hxl = 
        let hx1 = hxlHstAvl hxl occ
        match (hx1.A) with 
        | true -> 
                    let xyzOc1 = List.map (fun x -> x.C) occ
                    match hxl.A with 
                    | true -> 
                        let inc = hxl 
                                |> xyzAdjXyz 
                                |> List.except xyzOc1 
                                |> List.tryHead
                        match inc with 
                        | None -> { hxl with A = false }
                        | Some inc-> hxlHstAvl { 
                                                    A = true
                                                    B = false
                                                    C = Some inc |> Option.get 
                                                } occ
                    | false -> hxl
        | false -> hx1
        
    // Incremental Hexels
    let hxlIncHx2 (hxl : hxl list list) (occ : hxl list)  = 
        let rec hxInOgs1 (hxl : hxl list) (occ : hxl list) (acc : hxl list) = 
            match hxl with 
            | [] -> acc
            | a :: b -> 
                        let acc = acc @ [hxlIncHx1 a occ]
                        let occ = (acc @ occ) @ hxl
                        hxInOgs1 b occ acc
        
        let av1 = List.map (fun x -> List.filter (fun x -> x.A = true)x) hxl
        let lg1 = List.map (fun x -> List.isEmpty x) av1 
                |> List.contains true
        let hx1 = match lg1 with 
                    | true -> []
                    | false ->  let hd1 = List.map (fun x -> List.head x) av1
                                let in1 = hxInOgs1 hd1 occ [] |> List.chunkBySize 1
                                                    
                                let lc1 = [hxl;in1] 
                                        |> List.transpose 
                                        |> List.map (fun x -> List.concat x) 
                                        |> List.map (fun x -> List.distinct x)
                                lc1

        hx1 |> List.map (fun x -> hxlAvlChk x) |> List.map (fun x -> List.concat x)
    
    // Multiple Increments
    let hxlIncHx3 (hxl : hxl list list) (occ : hxl list) (cnt : int) : hxl list list = 
        
        let rec hxIncHx3 (hxl : hxl list list) (occ : hxl list) (cnt : int) = 
            let ttl = (List.length hxl) * cnt
            match (List.length (List.concat hxl) < ttl) || (List.length (List.concat hxl) = 0) with 
            | false -> hxl
            | true -> 
                    let hxl = hxlIncHx2 hxl occ
                    let occ = [occ; (hxlIncHx2 hxl occ) |> List.concat] |> List.concat |> hxlUnqAvl |> List.distinct
                    hxIncHx3  hxl occ cnt
        hxIncHx3 hxl occ cnt