
(* type constructors can be annotated with explicit regions *)
type `r1 ty_con_1 = string`r1

(* tuple types can be annotated with explicit regions *)
type `r2 ty_tuple_1 = (int * string)`r2

(* record types can be annotated with explicit regions *)
type `r3 ty_record_1 = {a:int, b:string}`r3

(* larger examples *)
type `[r1 r2] all = (int * bool)`r1 list`r2
