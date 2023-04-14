
(* type constructors can be annotated with explicit regions *)
type ty_con_1 = string`r1

(* tuple types can be annotated with explicit regions *)
type ty_tuple_1 = (int * string)`r2

(* record types can be annotated with explicit regions *)
type ty_record_1 = {a:int, b:string}`r3

(* larger examples *)
type all = (int * bool)`r1 list`r2
