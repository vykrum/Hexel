type Hxl = int * int * int

//Surrounding Hexels
let adjHxl hxl : Hxl =
    let a,b,c = hxl
    let x = [0; -1; -2; -1; 1; 2; 1] |> List.map (fun u -> u + a)
    let y = [0; -2; 0; 2; 2; 0; 2] |> List.map (fun v -> v + b)
    let z = [0; 0; 0; 0; 0; 0; 0] |> List.map (fun w -> w + c)
    List.zip3 x y z |> List.map Hxl

//Incremental Hexel
let incHxl (hxl : Hxl) (occ : Hxl list) : Hxl = 
    adjHxl hxl |> List.except occ |> List.head

let clsHxl (hxls,cnts : Hxl * int list ) 

let eg = (0,0,0) : Hxl
incHxl eg [0,0,0;-1,-2,0]