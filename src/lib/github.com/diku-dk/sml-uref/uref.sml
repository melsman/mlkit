
structure URef :> UREF =
struct

infix ::=

fun impossible () = raise Fail "URef: impossible"

datatype 'a urefC = ECR of 'a * int
                  | PTR of 'a uref
withtype 'a uref = 'a urefC ref

fun find (p as ref (ECR _)) = p
  | find (p as ref (PTR p')) = let val p'' = find p'
                               in p := PTR p''; p''
                               end

fun uref x = ref (ECR(x, 0))

fun !! p =
    case !(find p) of
        ECR(x,_) => x
      | _ => impossible()

fun update (p, x) =
    case find p of
	p' as ref(ECR(_, r)) => p' := ECR(x, r)
      | _ => impossible()

val op ::= = update

fun unify f (p, q) =
    case (find p, find q) of
	(p' as ref(ECR(pc, pr)), q' as ref(ECR(qc, qr))) =>
        let val newC = f (pc, qc)
        in if p' = q' then p' := ECR(newC, pr)
	   else if pr = qr
	   then (q' := ECR(newC, qr+1);
		 p' := PTR q')
	   else if pr < qr
	   then (q' := ECR(newC, qr);
		 p' := PTR q')
	   else (p' := ECR(newC, pr);  (* pr > qr *)
                 q':= PTR p')
        end
      | _ => impossible()

fun eq (p, p') = (find p = find p')

fun compare cmp (p, p') =
    case (find p, find p') of
	(p as ref (ECR(x,_)),p' as ref (ECR(x',_))) =>
	if p = p' then EQUAL else cmp (x,x')
      | _ => impossible()

end

(* Copyright 199x-2020 Fritz Henglein, Henning Niss *)
