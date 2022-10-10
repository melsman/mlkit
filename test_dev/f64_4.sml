local

infix  7  * / div mod
infix  6  + -
infix  4  = <> > >= < <=
infixr @ ::
infix :=

fun nil @ ys = ys
  | (x::xs) @ ys = x :: (xs @ ys)

fun printReal (n:real):unit = prim("printReal",n)

datatype t = R of real
val K = ref (R 0.0)

fun ! (ref a) = a
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))

fun work n =
    if n <= 0 then []
    else work (n-1) @ [n] @ work (n-1)
in
fun f (a,b,c:real,d,e,f,g,i) =
    let val t = a * b - c
        (*val () = K := (R c)*)
        val G =
            let val r = ref 10.0
            in fn a => !r + c
            end
        val l = work 10
    in t + d*e+f*g+i + G 5.0
    end

val () = printReal (f(5.0,2.0,3.0, 1.0,2.0,3.0,4.0,5.0))
end
