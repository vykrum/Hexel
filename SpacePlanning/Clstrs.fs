type XYZ = {X : int ; Y : int ; Z : double ; C : bool}

let hxVldOg (og : XYZ) = 
    match (og.X % 2 = 0) with 
    | true -> { X = og.X ; Y = og.Y - (og.Y % 4) + 1 ; Z = og.Z ; C = false }
    | false -> { X = og.X ; Y = og.Y - (og.Y % 4) + 3 ; Z = og.Z ; C = false }

// Adjacent Hexel Origins XY
let hxAdjOg (og : XYZ) = 
    List.map2 (fun x y -> { X = og.X + x; Y = og.Y + y ; Z = og.Z ; C = false })
        [-2;-1;1;2;1;-1] [0;2;2;0;-2;-2]

// Core Hexel Check
let hxCrChk (og : XYZ) (oc : XYZ list) = 
    match og.C with 
    | true -> og
    | false -> match List.except oc (hxAdjOg og) |> List.length = 0 with 
                | true -> {og with C = true}
                | false -> og

let hxIncOg (hs : XYZ list list) (oc : XYZ list) = 
    let a = [for i in hs -> match i with 
                            | [] -> []
                            | _ -> 
                                    let x1 = hxAdjOg i.[0]
                                            |> List.except (oc @ (List.concat hs)) 
                                            |> List.head
                                    let oc = oc @ (List.concat hs) @ [x1]
                                    [x1]]
    List.map2 (fun x y ->  x @ y) hs a

let a = hxAdjOg { X = 0; Y =1; Z = 0; C = false }
a
//let b = hxCrChk { X = 0; Y =1; Z = 0; C = false } a
//hxIncOg [[{X=0;Y=1;Z=0;C=false}];[{X= -2;Y=1;Z=0;C=false}];[{X= -1;Y=3;Z=0;C=false}]] ([{X= -3;Y=3;Z=0;C=false};{X= -4;Y=1;Z=0;C=false}])