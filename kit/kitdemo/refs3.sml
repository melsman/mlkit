    val it = let
                 val x: int ref = ref 3
                 val y: bool ref = ref true
                 val z: int ref  = if !y then x else ref 5
             in 
                z:= 6;
                !x
             end
