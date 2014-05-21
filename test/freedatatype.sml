(* This program is in fact legal according to The Definition, although
the program would go wrong if we were to execute it. There are two
ways to solve the problem:

  1. disallow type variables in the right-hand side of a datbind that
     are not bound on the left-hand side.

  2. disallow type variables in the right-hand side of a datbind that
     are not bound on the left-hand side or by some function binding.

The ML Kit implements strategy (1), which means that the program
freedatatype2.sml is not accepted by the ML Kit.

*)

let fun cast (a: 'a) : 'b = 
      let datatype t = C of 'c
      in case C a
           of C b => b
      end
in cast true + 5
end;
