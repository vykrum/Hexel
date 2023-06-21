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

// Non Uniform Increments
let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
    let ct0,hx0 = Array.unzip hxc
    let occ = [|hx0;occ|] |> Array.concat
    let hx1 = hx0 |> Array.map(fun x -> avl x occ)
    let hx2 = Array.map2 (fun x y -> {x with Cnt = y}) hx1 ct0
    let mxc = Array.max ct0
    let acc = Array.map (fun x -> Array.singleton x) hx1
    
    let rec nu1 (hxl :Hxl[]) (occ : Hxl[]) (mxc : int) (acc : Hxl[][]) = 
        match mxc with 
        | c when c < 1 -> acc
        | _ -> 
                let h01 = inc hxl occ
                let c01 = h01 |> Array.map (fun x -> x.Cnt)
                let acc = Array.map2 (fun x y -> Array.concat [|x;[|y|]|]) acc h01 
                let occ = [|h01;occ;hxl|] |> Array.concat |> Array.distinct
                let ac0 = acc |> Array.map (fun x -> Array.map(fun a -> avl a occ)x)
                let ac1 = ac0 |> Array.map(fun a -> Array.filter (fun x  -> x.Avl > 0)a)
                let ac2 = ac1 |> Array.map (fun x -> Array.head x)
                let h02 = Array.map2 (fun x y -> {x with Cnt = y}) ac2 c01
                let hxl = h02
                nu1 hxl occ (mxc-1) acc 
    (nu1 hx2 occ mxc acc) 

let a = {Avl = 6; Cnt = 1; Loc = 0,1,0}
let b = adj a
let c = [|[|a|];b|] |> Array.concat
//let d = inc b.[0..2] c
let e = nui (Array.zip [|2;2;2|] b[0..2]) c

