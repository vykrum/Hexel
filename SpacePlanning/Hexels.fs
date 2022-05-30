namespace Hexel

// Coordinates
type xyz = { X : int; Y : int; Z : double }

// Available, Boundary, Coordinates
type hxl = { A : bool; B : bool; C : xyz }

module Cluster =
    // Valid Hexel Coordinates
    let xyzVldLoc (loc : xyz) : xyz = 
        match (loc.X % 2 = 0) with 
        | true -> {
                        X = loc.X
                        Y = loc.Y - (loc.Y % 4) + 1
                        Z = loc.Z 
                    }
        | false -> { 
                        X = loc.X 
                        Y = loc.Y - (loc.Y % 4) + 3
                        Z = loc.Z 
                    }

    // Adjacent Hexel Locations
    let xyzAdjLoc (loc : hxl) : xyz list = 
        match loc.A with 
        | true -> List.map2 (fun x y -> { 
                                            X = loc.C.X + x
                                            Y = loc.C.Y + y
                                            Z = loc.C.Z 
                                        }
                            )
                            [-2;-1;1;2;1;-1] [0;2;2;0;-2;-2]
        | false -> []

    // Host Availability
    let hxlHstAvl (loc : hxl) (occ : hxl list) : hxl = 
        let xyzOc1 = List.map (fun x -> x.C) occ
        let lgt = xyzOc1 
                    |> List.except (loc.C :: xyzAdjLoc loc) 
                    |> List.length
        match (lgt = 0) with 
        | true -> { loc with A = false }
        | false -> loc

    // Incremental Hexel
    let hxlIncLoc (loc : hxl) (occ : hxl list) : hxl = 
        match (loc.A) with 
        | true -> 
                    let xyzOc1 = List.map (fun x -> x.C) occ
                    match loc.A with 
                    | true -> 
                        let inc = loc 
                                |> xyzAdjLoc 
                                |> List.except xyzOc1 
                                |> List.tryHead
                        match inc with 
                        | None -> { loc with A = false }
                        | Some inc-> hxlHstAvl { 
                                                    A = true
                                                    B = false
                                                    C = Some inc |> Option.get 
                                                } occ
                    | false -> loc
        | false -> loc
        
    // Incremental Hexels
    let hxlIncLcs (loc : hxl list list) (occ : hxl list) :  hxl list list list= 
        let rec hxInOgs1 (loc : hxl list) (occ : hxl list) = 
            match loc with 
            | [] -> []
            | a :: b -> 
                        let c = hxlIncLoc a occ
                        let occ = c :: occ
                        hxInOgs1 b occ @ [c]
        
        let av1 = List.map (fun x -> List.filter (fun x -> x.A = true)x) loc
        let lg1 = List.map (fun x -> List.isEmpty x) av1 
                |> List.contains true
        match lg1 with 
        | true -> []
        | false ->  let in1 = hxInOgs1 (List.map (fun x -> List.head x) av1) occ 
                                        |> List.chunkBySize 1
                    let lc1 = [loc;in1] 
                            |> List.transpose 
                            |> List.map (fun x -> List.concat x) 
                            |> List.map (fun x -> List.distinct x)
                    // Mark availability of output hexels
                    let oc1 = ((lc1 |> List.concat) @ occ) |> List.distinct
                    let lc2 = List.map (fun x -> (List.map (fun x -> hxlHstAvl x oc1))x) lc1
                    [lc2 ; [oc1]]


//let b = hxCrChk { X = 0; Y =1; Z = 0; C = false } a
//hxIncOg [[{X=0;Y=1;Z=0;C=false}];[{X= -2;Y=1;Z=0;C=false}];[{X= -1;Y=3;Z=0;C=false}]] ([{X= -3;Y=3;Z=0;C=false};{X= -4;Y=1;Z=0;C=false}])