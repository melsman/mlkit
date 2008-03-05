(* 
   Author: Martin Olsen (Version: 1.0)
   Major revision by Martin Elsman.
   Also modified to work with AJAX programming and SMLtoJs.
   mael 2008-02-05
*)

functor XMLrpc 
(X : sig 
       exception Connection of string (* should be the only exception raised 
                                       * by makeRequest and makeRequestAsync *)
       val makeRequest      : {url:string,request:string} -> string
       val makeRequestAsync : {url:string,request:string,cont:string -> unit} -> unit
     end) 
        :> XMLRPC = 

struct

    exception TypeConversion
          
    exception MethodInvocation of (int * string)

    exception ServerConnection of string
                              
    open WSeq
    infix &&
      
    (* Parsercombinator stuff *)
    open Parsercomb
        
    infix 7 |>
    infix 6 $-- --$ #-- --#
    infix 5 -- unless
    infix 3 >> >>*
    infix 2 >>=
    infix 0 ||
        
    type 'a pu = bool -> (('a -> WSeq.wseq) * ((char, 'a) parser))
    type 'a T = 'a pu
        
    local 
        fun elemU tag v = 
            ($ ("<" ^ tag ^ ">") -- repeat0 ($ "\n"))
            #-- v --# ($ ("</" ^ tag ^ ">") -- repeat0 ($ "\n"))

        fun elemP tag p =
            WSeq.$$["<", tag, ">"] && p && WSeq.$$["</", tag, ">"] 
        fun valueU v = elemU "value" v
        fun valueP p = elemP "value" p
        fun taggedValueU tag v = valueU (elemU tag v)
        fun taggedValueP tag p = valueP (elemP tag p) 
        fun param (toplevel:bool) value x = 
            if toplevel then elemP "param" (value x) else value x
        fun pu p u = fn toplevel => (param toplevel p, u) 
    in
        
        val int = 
            let val u = 
                    let val legalInt = Parsercomb.scan(Int.scan StringCvt.DEC)
                    in valueU (elemU "int" legalInt || elemU "i4" legalInt)
                    end
                val p = 
                    taggedValueP "int" o WSeq.$  o Int.toString
            in pu p u
            end
        
        val bool  = 
            let val u = taggedValueU "boolean" (Parsercomb.scan(Int.scan StringCvt.DEC) >> (fn x => x = 1))
                fun boolToStr b = if b then "1" else "0"
                val p = taggedValueP "boolean"  o WSeq.$ o boolToStr
            in pu p u
            end 
        
        val real = 
            let val u = taggedValueU "double" (Parsercomb.scan Real.scan)
                val p = taggedValueP "double"  o WSeq.$ o Real.toString
            in pu p u               
            end
        
        val string = 
            let val u = 
                    let fun legalCh #"&" = false 
                          | legalCh #"<" = false 
                          | legalCh _ = true 
                      val strConv = 
                          (getChar legalCh || ($ "&amp;" |> #"&") || ($ "&gt;" |> #">") || ($ "&lt;" |> #"<"))
                      val strU = Parsercomb.repeat0 strConv >> String.implode
                    in
                      valueU ((elemU "string" strU) || strU)
                    end
                fun strP #"<" = "&lt;" 
                  | strP #"&" = "&amp;"
                  | strP ch = String.str ch 
                val p = taggedValueP "string" o WSeq.$ o String.translate strP
            in pu p u
            end
        
        val date = 
            let val unpickle =  
                let fun getDate (((str, h), m), s) = 
                    let 
                        fun monthConv s = case s of 
                            "01" => Date.Jan 
                          | "02" => Date.Feb 
                          | "03" => Date.Mar 
                          | "04" => Date.Apr 
                          | "05" => Date.May 
                          | "06" => Date.Jun 
                          | "07" => Date.Jul 
                          | "08" => Date.Aug 
                          | "09" => Date.Sep 
                          | "10" => Date.Oct 
                          | "11" => Date.Nov 
                          | "12" => Date.Dec 
                          | _ => raise TypeConversion
                                
                        fun intFromStr s = let val v = Int.fromString s 
                                           in case v of 
                                               SOME i => i 
                                             | NONE => raise TypeConversion
                                           end                
                    in
                        Date.date {year = intFromStr (String.extract (str, 0, SOME 4)),
                                   month = (monthConv (String.extract (str, 4, SOME 2))),
                                   day =  intFromStr (String.extract (str, 6, SOME 2)),
                                   hour =  h,
                                   minute = m,
                                   second = s,
                                   offset = NONE}
                    end
                    val datePar = (scan (Int.scan StringCvt.DEC) >> Int.toString)
                        --$ "T" -- scan (Int.scan StringCvt.DEC) 
                        --$ ":" -- scan (Int.scan StringCvt.DEC) 
                        --$ ":" -- scan (Int.scan StringCvt.DEC) >> getDate
                in
                    taggedValueU  "dateTime.iso8601" datePar
                end
                val valueP = taggedValueP "dateTime.iso8601" o WSeq.$ o Date.fmt "%Y%m%dT%H:%M:%S"
            in pu valueP unpickle 
            end       

        fun conv (inj:'b->'a,prj:'a->'b) (a:'a pu) : 'b pu =
            let val (p,u) = a false
            in pu (p o inj) (u >> prj)
            end

        val unit =
            conv (fn () => 1,
                  fn 1 => ()
                   | _ => raise TypeConversion) int

        fun pair (a,b) =
            let val (pa,ua) = a false
                val (pb,ub) = b false
                fun pickler (x,y) = pa x && pb y
                val p = taggedValueP  "array" o (elemP "data" o pickler)
                val u = taggedValueU  "array" (elemU "data" (ua -- ub))
            in pu p u
            end

        fun tup3 (a,b,c) =
            let val (pa,ua) = a false
                val (pb,ub) = b false
                val (pc,uc) = c false
                fun pickler (x,y,z) = pa x && pb y && pc z
                val p = taggedValueP  "array" o (elemP "data" o pickler)
                val u = taggedValueU  "array" (elemU "data" (((ua -- ub) -- uc) >> (fn ((x,y),z) => (x,y,z))))
            in pu p u
            end

        fun tup4 (a,b,c,d) =
            let val (pa,ua) = a false
                val (pb,ub) = b false
                val (pc,uc) = c false
                val (pd,ud) = d false
                fun pickler (x,y,z,v) = pa x && pb y && pc z && pd v
                val p = taggedValueP  "array" o (elemP "data" o pickler)
                val unpickler =
                    (((ua--ub)--(uc--ud)) >> (fn ((x,y),(z,v)) => (x,y,z,v)))
                val u = taggedValueU  "array" (elemU "data" unpickler)
            in pu p u
            end
        
        fun list a =  
            let val (pick, unpick) = a false
                val u = taggedValueU  "array" (elemU "data" (repeat0 unpick))
                fun listpickler [] = WSeq.Empty 
                  | listpickler (y::ys) = pick y && listpickler ys
                val p = taggedValueP  "array" o (elemP "data" o listpickler)
            in pu p u
            end

        fun vector a = 
            conv (Vector.foldr (op ::) nil, 
                  Vector.fromList) (list a)
        
        fun array a = 
            conv (Array.foldr (op ::) nil, 
                  Array.fromList) (list a)
        
        fun option a =
            conv (fn SOME x => [x] 
                   | NONE => nil,
                  fn [x] => SOME x 
                   | nil => NONE 
                   | _ => raise TypeConversion) (list a)

        fun rpc0 a b method g = 
            let val  (pick,_) = a true
                val  (_, unpick)  = b true
                fun mkReq value = 
                    WSeq.flatten (WSeq.$$ ["<?xml version=\"1.0\"?><methodCall><methodName>", method, "</methodName><params>"] 
                                       && (pick value) && (WSeq.$ "</params></methodCall>") && WSeq.Nl && WSeq.Nl)                
                fun unwrap answ = 
                    let val methodResp = Substring.full(answ)
                        val (_, u) =  pair(int, string) false
                        fun fault (c, s) = raise MethodInvocation (c, s)
                        val faultU = elemU "fault" u >> fault  
                        val valueU = elemU "params" (elemU "param" unpick)
                    in
                        case Parsercomb.scanSubstr (elemU "methodResponse" (valueU || faultU)) methodResp of
                            SOME i => i 
                          | NONE => raise TypeConversion
                                handle Subscript => raise TypeConversion
                    end handle TypeConversion => raise Fail ("TC: " ^ answ)
            in g (mkReq,unwrap)
               handle e => case e of 
                             X.Connection str => raise ServerConnection str
                           | TypeConversion => raise e
                           | MethodInvocation _ => raise e
                           | Fail s => raise e
                           | _ => raise Fail ("rpc0 " ^ General.exnMessage e)
            end     

        fun rpc a b {url,method} = 
            rpc0 a b method
                 (fn (wr,uwr) => 
                  fn x => (uwr (X.makeRequest {url=url, request=wr x})))

        fun rpcAsync a b {url,method} =
            rpc0 a b method
                 (fn (wr,uwr) => 
                  fn x => 
                  fn f => X.makeRequestAsync {url=url, request=wr x, cont=fn y => f (uwr y)})
    end
end (* struct *)
