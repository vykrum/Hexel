// Hexel
type Hxl = { Avl : int; Cnt : int ; Loc : int * int * int }

// Adjacent Hexels
let adj (hxl : Hxl) = 
    match hxl.Avl with 
    | a when a < 1 -> hxl |> Array.singleton
    | _ -> 
            let (x,y,z) = hxl.Loc
            Array.map (fun c -> {Avl = 6 ; Cnt = 1; Loc = c})
                (Array.map3 (fun a b c ->
                    ((a + x), (b + y), (c + z)))
                    [|1; -1; -2; -1; 1; 2|]
                    [|-2; -2; 0; 2; 2; 0|]
                    [|0; 0; 0; 0; 0; 0|])

let loc (hxl : Hxl[]) = Array.map (fun x -> x.Loc) hxl
let cnt (hxl : Hxl[]) = Array.map (fun x -> x.Cnt) hxl

// Availability Update
let avl (hxl : Hxl) (occ : Hxl[]) = 
    let av1 = Array.except (loc occ) (loc(adj hxl))
                 |> Array.length
    {hxl with Avl = av1}

// Except Hexels
let exc (exl : Hxl[]) (hxl : Hxl[]) = 
    let exs hx  = ((Array.map (fun x -> x.Loc)exl) |> Array.contains hx.Loc) = false
    let hx1 = hxl |> Array.filter exs |> Array.distinct
    let hx2 = hx1 |> Array.groupBy (fun x -> x.Loc)
    let hx3 = hx2 |> Array.map (fun (x,y) -> 
                    match (y |> Array.length) with 
                    | 1 -> y |> Array.head
                    | _ -> {(y|> Array.head) with Avl = 0} )
    let hx4 = hx3 |> Array.map (fun x -> avl x ([|exl;hxl;hx3|] |> Array.concat))
    hx4

// Incremental Hexel
let icr (hxl : Hxl) (occ : Hxl[]) = 
    match hxl with 
    | hxl when hxl.Cnt < 1 -> {hxl with Cnt = 0}
    | hxl -> 
        let hx0 = avl hxl occ

        let hx1 = match hx0.Avl with 
                  | 0 -> Some hx0
                  | _ -> (exc (Array.concat [|[|hx0|];occ|]) (adj hx0)) |> Array.tryHead
        let hx2 = match hx1 with 
                  | None -> hx0
                  | Some x -> x
        let hx3 = avl hx2 ([|[|hx0|];[|hx2|];occ|] |> Array.concat)
        {hx3 with Cnt = hx3.Cnt - 1}

// Incremental Hexels
let inc (hxl : Hxl[]) (occ : Hxl[]) = 
    let hx1 = Array.scan (fun ac st ->
        let occ = [|[|icr ac occ|];occ|] |> Array.concat
        (icr st occ)) (Array.head hxl) hxl
    let oc1 = [|hxl;hx1;occ|] |> Array.concat |> Array.distinct
    let hx2 = Array.map (fun x -> avl x oc1) hx1
    hx2 |> Array.tail

(* // Non Uniform Increments
let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
    let ct0,hx0 = Array.unzip hxc
    let hx1 = Array.map2 (fun x y -> {x with Cnt = y}) hxo ct0
    let mxc = Array.max ct0
    let acc = Array.map (fun x -> Array.singleton x) hx1
    let rec nu1 (hxl :Hxl[]) (occ : Hxl[]) (mxc : int) (acc : Hxl[][]) = 
        match mxc with 
        | c when c < 1 -> acc
        | _ -> 
                let hxc = inc hxc occ
                let ct,hx = Array.unzip hxc
                let occ = [|(Array.concat acc);occ|] |> Array.concat
                let acc = Array.map2 (fun x y -> [x ; [|y|]]|> Array.concat) acc hx
                
                nu1 hxc occ (mxc-1) acc 
    (nu1 hxc occ mxc acc) |> Array.map (fun x -> Array.distinct x) *)
    

let a = {Avl = 6; Cnt = 1; Loc = 0,1,0}
let b = adj a
let c = [|[|a|];b|] |> Array.concat
let d = inc b.[0..2] c

