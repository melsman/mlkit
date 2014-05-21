fun take([], i) = []
  | take(x::xs, i) = 
       if i>0 then x::take(xs, i-1)
       else []
fun drop(l as [], _) = l
  | drop(l as (x::xs),i) = if i>0 then drop(xs,i-1) else l;