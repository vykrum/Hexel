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

    // Except Hexels
    let exc (exl : Hxl[]) (hxl : Hxl[]) = 
        let cts hx  = ((Array.map (fun x -> x.Loc)exl) 
            |> Array.contains hx.Loc) = false
        let hx1 = hxl |> Array.filter cts |> Array.distinct
        let hx2 = hx1 |> Array.groupBy (fun x -> x.Loc)
        hx2 |> Array.map (fun (x,y) -> 
                match (y.Length) with 
                | 1 -> y |> Array.head
                | _ -> {Avl = 0 ; Loc= x} )

    // Available Hexels
    let chk (hxl : Hxl) (occ : Hxl[]) = 
        match hxl.Avl with
        | 0 -> hxl
        | x -> 
                    let av = (exc ([|[|hxl|];occ|]|> Array.concat) (adj hxl)) |> Array.length
                    match av with 
                    | 0 -> {hxl with Avl = 0}
                    | _ -> {hxl with Avl = av}
    

    // Incremental Hexels
    let inc (hxl : (int * Hxl)[]) (occ : Hxl[]) = 
        let hxc = hxl |> Array.map ( fun (x,y) -> 
            match x with
            | x when x < 1 -> 0, (y |> Array.singleton)
            | x -> (x-1), (adj y))
        let (_,hx1) = hxl |> Array.unzip
        let (ct2 , hx2) = hxc |> Array.unzip
        let oc1 = [|occ ; hx1|] |> Array.concat
        let hx3 = hx2 |> Array.scan  (fun ac st-> 
                let oc1 = [|ac ; oc1|] |> Array.concat
                let h01 = (exc oc1 st)
                let h02 = h01 |> Array.tryHead
                match h02 with 
                | Some h02 -> [|h02|]
                | None -> [|st |> Array.head|]) [||]
                |> Array.tail 
                |> Array.map (fun x -> Array.head x)
        Array.zip ct2 hx3

// Non Uniform Clusters
    let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
            let (ct1,hx1) = hxc|> Array.unzip
            let mxc = (Array.max ct1) + 1
            let acc = (Array.map(fun x -> Array.singleton x)hx1)
            let rec nux (hxc : (int * Hxl)[]) (occ : Hxl[]) (mxc : int) (acc : Hxl[][]) = 
                match mxc with 
                | mxc when (mxc <= 1) -> acc
                | _ ->
                        let h01 = inc hxc occ
                        let (c02,h02) = h01 |> Array.unzip
                        let ac1 = Array.map2 (fun x y -> [x ; [|y|]]|> Array.concat) acc h02
                        let oc1 = [|(Array.concat ac1) ; occ|] |> Array.concat |> Array.distinct
                        let ac2 = ac1 |> Array.map (fun x -> Array.map (fun y -> chk y oc1)x)
                        let acc = ac2 |> Array.map (fun x -> Array.distinct x)
                        let ac3 = acc |> Array.map (fun x -> Array.filter (fun y -> y.Avl > 0)x)
                        let h00 = (Array.map (fun x -> Array.tryHead x ) ac3)
                        let h02 = Array.map2 (fun x y -> match x with 
                                                                    | Some x -> x
                                                                    | None -> y) h00 h02
                        let hxc = Array.zip c02 h02
                        let occ = [occ ; (acc |> Array.concat) ; h02] |> Array.concat |> Array.distinct
                        nux hxc occ (mxc-1) acc
            (nux hxc occ mxc acc) |> Array.map(fun x -> Array.distinct x)


    // Testing
    let hxx1 = {Avl = 6 ; Loc = 0,1,0}
    let hxx2 = adj hxx1
    //let hxx3 = [|hxx2.[1];hxx2.[3];hxx2.[5]|]
    nui (Array.zip [|6;6;6;6;6;6|] hxx2[1..6]) hxx2