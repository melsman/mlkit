   datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree

   (* preorder traversal of tree *)

   fun preord (Lf, xs) = xs
     | preord (Br(x,t1,t2),xs) = 
         x::preord(t1,preord(t2,xs))

   (* building a balanced binary tree
      from a list: *)

   fun balpre [] = Lf
     | balpre(x::xs) = 
        let val k = length xs div 2
        in Br(x, balpre(take(xs, k)),
                 balpre(drop(xs, k)))
        end

   (* preord o balpre is the identity: *)

   val it = print(implode(preord(balpre(explode 
       "Greetings from the Kit\n"),[])));
       
