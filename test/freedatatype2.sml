(* The program freedatatype2.sml is legal according to The Definition, but
would go wrong if we were to execute it. There are two ways to 
solve the problem:

  1. disallow type variables in the right-hand side of a datbind that are
     not bound on the left-hand side.

  2. disallow type variables in the right-hand side of a datbind that are
     not bound on the left-hand side or by some function binding.

Following (2), the program below should type check and execute. However, the 
ML Kit implements (1), which means that the program does not type check.
*)


let fun id (a: 'a) : 'a = 
      let datatype t = C of 'a
      in case C a
           of C b => b
      end
in id 3 + 5
end;
