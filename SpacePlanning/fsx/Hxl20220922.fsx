// Hexel
type Hxl = { Avl : int; Loc : int * int * int }

// Adjacent Hexels
let adj (hxl : Hxl) = 
    match hxl.Avl with 
    | x when x<1 -> hxl |> Array.singleton
    | _ -> 
            let (x,y,z) = hxl.Loc
            Array.map (fun x -> {Avl = 6 ; Loc= x}) 
                (Array.map3 (fun a b c -> 
                    ((a + x), (b + y), (c + z))) 
                    [|0; 1; -1; -2; -1; 1; 2|] 
                    [|0; -2; -2; 0; 2; 2; 0|] 
                    [|0; 0; 0; 0; 0; 0; 0|])

// Available Hexels
let avb (occ : Hxl[]) (hxl : Hxl) = 
    match hxl.Avl with
    | x when x<1 -> {hxl with Avl = 0}
    | _ -> 
            let exp = ([|[|hxl|];occ|]|> Array.concat) |> Array.map (fun x -> x.Loc) |> Array.distinct
            let ajc = (adj hxl) |> Array.map (fun x -> x.Loc)
            let avl = (Array.except exp ajc) |> Array.length
            {hxl with Avl = avl}

// Except Hexels
let exc (exl : Hxl[]) (hxl : Hxl[]) = 
    let exs hx  = ((Array.map (fun x -> x.Loc)exl) |> Array.contains hx.Loc) = false
    let hx1 = hxl |> Array.filter exs |> Array.distinct
    let hx2 = hx1 |> Array.groupBy (fun x -> x.Loc)
    let hx3 = hx2 |> Array.map (fun (x,y) -> 
                    match (y |> Array.length) with 
                    | 1 -> y |> Array.head
                    | _ -> {Avl = 0 ; Loc= x} )
    hx3 |> Array.map (fun x -> avb ([|exl;hxl;hx3|] |> Array.concat) x)

let exq (hxl : Hxl[]) (occ : Hxl[]) = 
    let hx1 = hxl |> Array.map (fun x -> (adj x))
    let hx2 = hx1 |> Array.map (fun x -> exc ([|occ;hxl|] |> Array.concat) x)
    let hx2a = hx2 |> Array.windowed 2 |> Array.map (fun x -> exc x.[0] x.[1]) |> Array.map (fun x -> Array.tryHead x)
    let hx3 = Array.map2 ( fun x y -> match x with 
                                        | Some x -> x
                                        | None -> y) hx2a (hxl |> Array.tail)
    let hx4 = [|[|(hx2 |> Array.head) |> Array.head|];hx3|] |> Array.concat
    let in1 = [|0..(Array.length hxl)-1|]
    let hx5 = in1 |> Array.map (fun x -> Array.removeAt x hx4)
    let hx6 = Array.map2 (fun x y -> exc y [|x|]) hx4 hx5
    let hx7 = Array.map2 (fun x y -> match y with 
                                        | [||] -> x
                                        | _ -> y |> Array.head)hxl hx6
    hx7 |> Array.map (fun x -> avb ([|occ;hxl;hx7|] |> Array.concat) x)

// Incremental Hexels
let inc (hxl : (int * Hxl)[]) (occ : Hxl[]) = 
    let (ct0,hx0) = hxl |> Array.unzip
    let hx1 = exq hx0 ([|occ;hx0|]|>Array.concat)
    let hx3 = Array.map3 ( fun x y z -> 
            match z with
            | z when z < 1 -> 0, x 
            | z -> (z-1), y) hx0 hx1 ct0
    let (ct1,hx4) = hx3 |> Array.unzip
    let hx5 = hx4 |> Array.map (fun x -> avb ([|occ;hx0;hx4|] |> Array.concat) x)
    Array.zip ct1 hx5

// Non Uniform Clusters
let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
            let (ct1,hx1) = hxc|> Array.unzip
            let mxc = (Array.max ct1) + 1
            let acc = (Array.map (fun x -> Array.singleton x)hx1)
            let rec nux (hxc : (int * Hxl)[]) (occ : Hxl[]) (mxc : int) (acc : Hxl[][]) = 
                match mxc with 
                | mxc when (mxc <= 1) -> acc
                | _ ->
                        let h01 = inc hxc occ
                        let (c02,h02) = h01 |> Array.unzip
                        let ac1 = Array.map2 (fun x y -> [x ; [|y|]]|> Array.concat) acc h02
                        let oc1 = [|(Array.concat ac1) ; occ|] |> Array.concat |> Array.distinct
                        let ac2 = ac1 |> Array.map (fun x ->Array.map (fun y -> avb oc1 y)x)
                        let acc = ac2 |> Array.map (fun x -> Array.distinct x)
                        let ac3 = acc |> Array.map (fun x -> Array.filter (fun y -> y.Avl > 0)x)
                        let h00 = (Array.map (fun x -> Array.tryHead x ) ac3)
                        let h02 = Array.map2 (fun x y -> match x with 
                                                                    | Some x -> x
                                                                    | None -> y) h00 h02
                        let hxc = Array.zip c02 h02
                        let occ = [occ ; (acc |> Array.concat) ; h02] |> Array.concat |> Array.distinct
                        nux hxc occ (mxc-1) acc
            (nux hxc occ mxc acc) 

// Testing
let hxx1 = {Avl = 6 ; Loc = 0,1,0}
let hxx2 = adj hxx1
//let hxx3 = [|hxx2.[1];hxx2.[3];hxx2.[5]|]
nui (Array.zip [|0;0;0;8;8;8|] hxx2[1..6]) hxx2