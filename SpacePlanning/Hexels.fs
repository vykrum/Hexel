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
        let lgt = xyzOc1 
                    |> List.except (hxl.C :: xyzAdjXyz hxl) 
                    |> List.length
        match (lgt = 0) with 
        | true -> { hxl with A = false }
        | false -> hxl

    // Incremental Hexel
    let hxlIncXyz (hxl : hxl) (occ : hxl list) : hxl = 
        match (hxl.A) with 
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
        | false -> hxl
        
    // Incremental Hexels
    let hxlInXyzs (hxl : hxl list list) (occ : hxl list) :  hxl list list list= 
        let rec hxInOgs1 (hxl : hxl list) (occ : hxl list) = 
            match hxl with 
            | [] -> []
            | a :: b -> 
                        let c = hxlIncXyz a occ
                        let occ = c :: occ
                        hxInOgs1 b occ @ [c]
        
        let av1 = List.map (fun x -> List.filter (fun x -> x.A = true)x) hxl
        let lg1 = List.map (fun x -> List.isEmpty x) av1 
                |> List.contains true
        match lg1 with 
        | true -> []
        | false ->  let in1 = hxInOgs1 (List.map (fun x -> List.head x) av1) occ 
                                        |> List.chunkBySize 1
                    let lc1 = [hxl;in1] 
                            |> List.transpose 
                            |> List.map (fun x -> List.concat x) 
                            |> List.map (fun x -> List.distinct x)
                    // Mark availability of output hexels
                    let oc1 = ((lc1 |> List.concat) @ occ) |> List.distinct
                    let lc2 = List.map (fun x -> (List.map (fun x -> hxlHstAvl x oc1))x) lc1
                    [lc2 ; [oc1]]



    // Test Zone
    // Sample initial host origins 
    let smXyLc1 = { X=0;Y=0;Z=0 } |> xyzVldXyz
    let smHxLc1 = { A = true; B = false; C = smXyLc1}
    let smHxOc1 = xyzAdjXyz smHxLc1 |> List.map (fun x -> {A = true; B = false; C = x})
    let smHxOc2 = smHxLc1 :: smHxOc1
    let smHxLc2 = List.take 3 smHxOc1 |> List.chunkBySize 1
    let a = (hxlInXyzs smHxLc2 smHxOc2).[1].[0]
    //hxlMubloc cnt
    //hxlMulLcs smHxLc2 smHxOc2 10 