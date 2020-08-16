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

//Incremental Hexel
let incHxl (hxl : Hxl list) (occ : Hxl list) : Hxl list= 
    let inc = hxl 
            |> prmHxl 
            |> List.head 
            |> adjHxl 
            |> List.except (hxl @ occ) 
            |> List.head 
    List.append hxl [inc] 

List.fold (fun a b -> a + string(b)) "" [1;2;3;4;5]

//let clsHxl (hxls,cnts : Hxl * int list ) (occ : Hxl list) = 

incHxl [-3,-2,0; -4,-4,0; -5,-2,0] [0,0,0; -1,-2,0; -2,0,0; -1,2,0; 1,2,0; 2,0,0; 1,-2,0; -2,-4,0; -3,-2,0]