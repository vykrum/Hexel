type Hxl = 
    | Host of int * int * double
    | Nost of int * int * double

let adj (hst : Hxl) = 
    let vld:Hxl = 
        match hst with 
        | Host (x,y,z) when (x % 2 = 0) -> Host (x, y - (y % 4) + 1, z)
        | Host (x,y,z) -> Host (x, y-(y%4) + 3, z)
        | Nost (x,y,z) when (x % 2 = 0) -> Nost (x, y - (y % 4) + 1, z)
        | Nost (x,y,z) -> Nost (x, y-(y%4) + 3, z)

    match vld with 
    | Host (x,y,z) -> 
        List.map2 (fun a b -> 
            Host (x + a, (y - (y % 4) + 1) + b, z) ) 
            [0; -2; -1; 1; 2; 1; -1] 
            [0; 0; 2; 2; 0; -2; -2]
    | Nost _ -> [vld]

let inc (hst : Hxl) (occ : Hxl list) = 
    match hst with 
    | Nost _ -> adj hst 
                |> List.head
    | Host (x,y,z) when 
        (List.except  occ (adj hst) 
        |> List.length) = 0 
        -> adj (Nost (x,y,z)) 
            |> List.head
    | Host _ -> List.except occ (adj hst) 
                |> List.tail
                |> List.head

inc (Host (0,0,0)) (adj(Host (0,0,0)))