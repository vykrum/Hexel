module Hexel =
    
    type Hxl = 
        | OG of int []
        | OP of int []

    type Sqn = 
        // Vertical,Horizontal,Clockwise,Anticlockwise,North,South,East,West
        | VCEE
    let sequence 
        (sqn:Sqn) =  
        match sqn with 
        | VCEE -> array2D [ [0x0;0x0]; [0x2;0x0]; [0x1;0xFFFFFFFE]; [0xFFFFFFFF;0xFFFFFFFE]; [0xFFFFFFFE;0x0]; [0xFFFFFFFF;0x2]; [0x1;0x2]] 

    // Identity Hexel
    let identity = 
        OG ([|0x0; 0x0; 0x0|])

    // Get Coordinates
    let hxlCrd 
        (hxl : Hxl) = 
        match hxl with 
        | OG a -> a
        | OP a -> a

    // Standardize type
    let allOG 
        (hxo:Hxl[]) = 
        
        hxo
        |> Array.map(fun x -> hxlCrd x)
        |> Array.map(fun x -> OG x)

    // Get Hexel from Tuple
    let getHxls 
        (hxo : (Hxl*int)[]) = 
        
        hxo
        |> Array.map(fun x 
                        -> fst x)
                        
   
