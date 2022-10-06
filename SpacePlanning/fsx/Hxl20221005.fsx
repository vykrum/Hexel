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

let loc (hxl : Hxl[]) =  Array.map (fun x -> x.Loc) hxl

let avl (hxl : Hxl) (occ : Hxl[]) = 
    let av1 = Array.except (loc occ) (loc(adj hxl))
                 |> Array.length
    {hxl with Avl = av1}

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

let inc (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
        Array.scan (fun ac st ->
            let occ = [|[|snd (icr ac occ)|];occ|] |> Array.concat
            (icr st occ)) (Array.head hxc) hxc
        |> Array.tail


let a = adj {Avl = 6; Loc = 0,1,0}
let b = snd(icr (3,a.[1]) a)
let c = inc (Array.zip [|0;3;0|] a[1..3]) a

