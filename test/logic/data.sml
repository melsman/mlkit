(* data.sml *)

structure Data =
struct
  local
    open Term Trail Unify
    val cons_s = "cons"
    val x_s = "x"
    val nil_s = "nil"
    val o_s = "o"
    val s_s = "s"
    val CON_o_s = CON(o_s)
    val CON_nil_s = CON(nil_s)
    val CON_x_s = CON(x_s)
  in
      fun exists sc = sc (REF(ref(NONE)))

fun move_horiz (T_1, T_2) sc = 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
(
trail (fn () => 
exists (fn T => 
exists (fn TT => 
unify (T_1, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, T])])]), TT])) (fn () => 
unify (T_2, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, T])])]), TT])) (fn () => 
sc ())))))
;
exists (fn P1 => 
exists (fn P5 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () => 
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () => 
sc ())))))
))
;
exists (fn P1 => 
exists (fn P2 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, CON_nil_s])])])])]), TT])) (fn () => 
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), TT])) (fn () => 
sc ())))))
))
;
exists (fn L1 => 
exists (fn P4 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () => 
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () => 
sc ())))))
))
;
exists (fn L1 => 
exists (fn P1 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, CON_nil_s])])])]), TT])])) (fn () => 
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])])]), TT])])) (fn () => 
sc ())))))
))
;
exists (fn L1 => 
exists (fn L2 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, CON_nil_s])])]), TT])])])) (fn () => 
unify (T_2, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), TT])])])) (fn () => 
sc ())))))
))
;
exists (fn T => 
exists (fn TT => 
unify (T_1, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, T])])]), TT])) (fn () => 
unify (T_2, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, T])])]), TT])) (fn () => 
sc ()))))
))
;
exists (fn P1 => 
exists (fn P5 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () => 
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [P5, CON_nil_s])])])])]), TT])) (fn () => 
sc ())))))
))
;
exists (fn P1 => 
exists (fn P2 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), TT])) (fn () => 
unify (T_2, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [P2, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, CON_nil_s])])])])]), TT])) (fn () => 
sc ())))))
))
;
exists (fn L1 => 
exists (fn P4 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () => 
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, STR(cons_s, [P4, CON_nil_s])])])]), TT])])) (fn () => 
sc ())))))
))
;
exists (fn L1 => 
exists (fn P1 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])]), TT])])) (fn () => 
unify (T_2, STR(cons_s, [L1, STR(cons_s, [STR(cons_s, [P1, STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, CON_nil_s])])])]), TT])])) (fn () => 
sc ())))))
))
;
exists (fn L1 => 
exists (fn L2 => 
exists (fn TT => 
unify (T_1, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), TT])])])) (fn () => 
unify (T_2, STR(cons_s, [L1, STR(cons_s, [L2, STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_o_s, CON_nil_s])])]), TT])])])) (fn () => 
sc ())))))
)

and rotate (T_1, T_2) sc = 
exists (fn P11 => 
exists (fn P12 => 
exists (fn P13 => 
exists (fn P14 => 
exists (fn P15 => 
exists (fn P21 => 
exists (fn P22 => 
exists (fn P23 => 
exists (fn P24 => 
exists (fn P31 => 
exists (fn P32 => 
exists (fn P33 => 
exists (fn P41 => 
exists (fn P42 => 
exists (fn P51 => 
unify (T_1, STR(cons_s, [STR(cons_s, [P11, STR(cons_s, [P12, STR(cons_s, [P13, STR(cons_s, [P14, STR(cons_s, [P15, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [P21, STR(cons_s, [P22, STR(cons_s, [P23, STR(cons_s, [P24, CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [P31, STR(cons_s, [P32, STR(cons_s, [P33, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [P41, STR(cons_s, [P42, CON_nil_s])]), STR(cons_s, [STR(cons_s, [P51, CON_nil_s]), CON_nil_s])])])])])) (fn () => 
unify (T_2, STR(cons_s, [STR(cons_s, [P51, STR(cons_s, [P41, STR(cons_s, [P31, STR(cons_s, [P21, STR(cons_s, [P11, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [P42, STR(cons_s, [P32, STR(cons_s, [P22, STR(cons_s, [P12, CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [P33, STR(cons_s, [P23, STR(cons_s, [P13, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [P24, STR(cons_s, [P14, CON_nil_s])]), STR(cons_s, [STR(cons_s, [P15, CON_nil_s]), CON_nil_s])])])])])) (fn () => 
sc ())))))))))))))))))

and move (T_1, T_2) sc = 
(
trail (fn () => 
(
trail (fn () => 
exists (fn X => 
exists (fn Y => 
unify (T_1, X) (fn () => 
unify (T_2, Y) (fn () => 
move_horiz (X, Y) sc)))))
;
exists (fn X => 
exists (fn X1 => 
exists (fn Y => 
exists (fn Y1 => 
unify (T_1, X) (fn () => 
unify (T_2, Y) (fn () => 
rotate (X, X1) (fn () => 
move_horiz (X1, Y1) (fn () => 
rotate (Y, Y1) sc))))))))
))
;
exists (fn X => 
exists (fn X1 => 
exists (fn Y => 
exists (fn Y1 => 
unify (T_1, X) (fn () => 
unify (T_2, Y) (fn () => 
rotate (X1, X) (fn () => 
move_horiz (X1, Y1) (fn () => 
rotate (Y1, Y) sc))))))))
)

and solitaire (T_1, T_2, T_3) sc = 
(
trail (fn () => 
exists (fn X => 
unify (T_1, X) (fn () => 
unify (T_2, STR(cons_s, [X, CON_nil_s])) (fn () => 
unify (T_3, INT(0)) (fn () => 
sc ())))))
;
exists (fn N => 
exists (fn X => 
exists (fn Y => 
exists (fn Z => 
unify (T_1, X) (fn () => 
unify (T_2, STR(cons_s, [X, Z])) (fn () => 
unify (T_3, STR(s_s, [N])) (fn () => 
move (X, Y) (fn () => 
solitaire (Y, Z, N) sc))))))))
)

and solution1 (T_1) sc = 
exists (fn X => 
unify (T_1, X) (fn () => 
solitaire (STR(cons_s, [STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s,
 CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])]), STR(cons_s, [STR(cons_s, [CON_x_s, CON_nil_s]), CON_nil_s])])])])])
, X, STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [INT(0)])])])])])])])])])])])])])) sc))

and solution2 (T_1) sc = 
exists (fn X => 
unify (T_1, X) (fn () => 
solitaire (STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s,
 CON_nil_s])])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_o_s, STR(cons_s, [CON_x_s, CON_nil_s])])]), STR(cons_s, [STR(cons_s, [CON_x_s, STR(cons_s, [CON_x_s, CON_nil_s])]), STR(cons_s, [STR(cons_s, [CON_x_s, CON_nil_s]), CON_nil_s])])])])])
, X, STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [STR(s_s, [INT(0)])])])])])])])])])])])])])) sc))
  end (* local *)
end; (* Data *)
