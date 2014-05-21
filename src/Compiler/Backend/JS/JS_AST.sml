signature JS_AST = sig

  type id = string  (* labels and identifiers *)

  datatype cnst = Int of Int32.int | Str of string | Real of string
                | Bool of bool | Word of Word32.word | Null

  datatype stmt =
         Var of id * exp option
       | Exp of exp
       | Seq of stmt list
       | Sw of exp * (cnst * stmt) list * stmt option
       | Return of exp
       | Continue of id
       | While of id option * exp * stmt   (* the optional id is a label *)
       | IfStmt of exp * stmt * stmt option
       | Break
       | Try of stmt * id * stmt
       | Throw of exp
       | Embed of string

       and exp =
         Prim of string * exp list   (* string determines if it is infix *)
       | Array of exp list
       | IfExp of exp * exp * exp
       | Fun of id list * stmt 
       | App of exp * exp list
       | Id of id
       | Cnst of cnst
       | Prop of exp * id
       | New of id * exp list
       | Sub of exp * exp

  val is_infix : string -> bool 

  val pr_cnst : cnst -> string
  val pr_stmt : stmt -> string
  val pr_exp : exp -> string  
end

