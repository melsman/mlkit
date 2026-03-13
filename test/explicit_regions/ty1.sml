
(* type constructors can be annotated with explicit regions *)
type ty_con_1 `r1 = string`r1

(* tuple types can be annotated with explicit regions *)
type ty_tuple_1 `r2 = (int * string)`r2

(* record types can be annotated with explicit regions *)
type ty_record_1 `r3 = {a:int, b:string}`r3

(* larger examples *)
type all `[r1 r2] = (int * bool)`r1 list`r2
