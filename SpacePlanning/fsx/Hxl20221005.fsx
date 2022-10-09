// Hexel
type Hxl = { Avl : int; Loc : int * int * int }

// Adjacent Hexels
let adj (hxl : Hxl) = 
    match hxl.Avl with 
    | a when a < 1 -> hxl |> Array.singleton
    | _ -> 
            let (x,y,z) = hxl.Loc
            Array.map (fun c -> {Avl = 6 ; Loc = c})
                (Array.map3 (fun a b c ->
                    ((a + x), (b + y), (c + z)))
                    [|0; 1; -1; -2; -1; 1; 2|]
                    [|0; -2; -2; 0; 2; 2; 0|]
                    [|0; 0; 0; 0; 0; 0; 0|])

let loc (hxl : Hxl[]) = Array.map (fun x -> x.Loc) hxl

// Availability Update
let avl (hxl : Hxl) (occ : Hxl[]) = 
    let av1 = Array.except (loc occ) (loc(adj hxl))
                 |> Array.length
    {hxl with Avl = av1}

// Incremental Hexel
let icr (hxc : int * Hxl) (occ : Hxl[]) = 
    match hxc with 
    | c,hxl when c<1 -> 0,hxl
    | c,hxl -> 
        let hx0 = avl hxl occ
        let hx1 = match hx0.Avl with 
                  | 0 -> Some hx0
                  | _ -> (Array.except occ (adj hxl)) |> Array.tryHead
        let hx2 = match hx1 with 
                  | None -> hx0
                  | Some x -> x
        let hx3 = avl hx2 ([|[|hx0|];[|hx2|];occ|] |> Array.concat)
        ((c-1),hx3)

// Incremental Hexels
let inc (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
    let hc1 = Array.scan (fun ac st ->
        let occ = [|[|snd (icr ac occ)|];occ|] |> Array.concat
        (icr st occ)) (Array.head hxc) hxc
              |> Array.tail
    let (ct0,hx0) = Array.unzip hxc
    let (ct1,hx1) = Array.unzip hc1
    let oc1 = [|hx0;hx1;occ|] |> Array.concat |> Array.distinct
    let hx2 = Array.map (fun x -> avl x oc1) hx1
    Array.zip ct1 hx2

// Non Uniform Increments
let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
    let cnt,hx1 = Array.unzip hxc
    let mxc = Array.max cnt
    let acc = Array.map (fun x -> Array.singleton) hx1
    let rec nu1 (hxc : (int * Hxl)[]) (occ : Hxl[]) (mxc : int) (acc : Hxl[][]) = 
        match mxc with 
        | c when c < 1 -> acc
        | _ -> 
                let hxc = inc hxc occ
                let _,h01 = Array.unzip hxc
                let acc = Array.map2 (fun x y -> [x ; [|y|]]|> Array.concat) acc h01
                let occ = [|(Array.concat acc);occ|] |> Array.concat
                nu1 hxc occ (mxc-1) acc 
    nu1 hxc occ acc mxc
    

let a = adj {Avl = 6; Loc = 0,1,0}
let b = snd(icr (3,a.[1]) a)
let c = nui (Array.zip [|1..6|] [|a.[3];a.[1];a.[4];a.[5];a.[2];a.[6]|]) a

