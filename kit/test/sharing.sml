  signature S = sig
    type t = int
    type u
  end

  functor F (structure S1 : S
             structure S2 : S
             sharing S1 = S2) = struct 
  end
