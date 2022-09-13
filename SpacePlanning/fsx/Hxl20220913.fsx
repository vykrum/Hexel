type Hxl = {
    Avl : bool
    Loc : int * int * int
}

let adj (hxl : Hxl) = 
    match hxl.Avl with 
    | false -> hxl |> List.singleton
    | true -> 
            let (x1,y1,z1) = hxl.Loc
            let x2 = List.map (fun x -> x1 + x) [0; -2; -1; 1; 2; 1; -1] 
            let y2 = List.map (fun y -> y1 + y) [0; 0; 2; 2; 0; -2; -2]
            let z2 = List.map (fun z -> z1 + z) [0; 0; 0; 0; 0; 0; 0]
            List.map (fun x -> {Avl = true ; Loc= x}) (List.zip3 x2 y2 z2)

let exc (hxl : Hxl list) (exl : Hxl List) = 
    let cts hx  = ((List.map (fun x -> x.Loc)exl) |> List.contains hx.Loc) = false
    hxl |> List.filter cts

let inc (hxl : Hxl list) (occ : Hxl list) = 
    let hx1 = List.map (fun x -> adj x) hxl
    hx1



// Testing
let Loc1 hxl = List.map (fun x -> x.Loc) hxl
let occ = (adj {Avl=true ; Loc=(0,1,0)})
let hxx = (inc (occ[1..3]) occ) |> List.map(fun x -> Loc1 x)

let r = exc occ occ[1..2]