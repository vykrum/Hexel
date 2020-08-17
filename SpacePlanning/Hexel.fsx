type Hxl = int * int * int

//Surrounding Hexels
let adjHxl (hxl : Hxl) =
    let a,b,c = hxl
    let x = [-1; -2; -1; 1; 2; 1] 
            |> List.map (fun u -> u + a)
    let y = [-2; 0; 2; 2; 0; -2] 
            |> List.map (fun v -> v + b)
    let z = [0; 0; 0; 0; 0; 0] 
            |> List.map (fun w -> w + c)
    List.zip3 x y z |> List.map Hxl

// Perimeter cells
let prmHxl (ce : Hxl list) =
    // ce : All cells in cluster
    List.filter (fun x -> 
        adjHxl x |> List.map (fun x -> 
            List.contains x ce) 
            |> List.contains false) ce

// Incremental Hexel
let incHxl (occ : Hxl list) (hxl : Hxl): Hxl list = 
    let inc = hxl
            |> adjHxl 
            |> List.except (hxl :: occ) 
            |> List.head 
    [inc ; hxl]

// Incremental Cluster
let incCls (occ : Hxl list) (hxl : Hxl list) = 
        let a = List.scan incHxl occ hxl
                |> List.tail
                |> List.map List.head
        List.map2 (fun x y -> x :: [y]) a hxl

incCls [0,0,0; -1,-2,0; -2,0,0] [-1,2,0; 1,2,0; 2,0,0]
// incHxl [-4,-4,0; -5,-2,0]  (-3,-2,0)
adjHxl (0,0,0)