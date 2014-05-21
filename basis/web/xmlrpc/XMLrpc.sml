(* 
   Author: Martin Olsen (Version: 1.0)
   Major revision by Martin Elsman.
   mael 2008-02-05
*)

functor XMLrpc 
(X : sig exception Connection of string (* should be the only exception raised in makeRequest *)

         (* for client functionality *)
         val makeRequest : string (* url *) -> string (* request *) -> string (* response *)

         (* for server functionality *)
         val getRequestData : unit -> string
         val write : string -> unit

(*       val log : string -> unit *)
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
        fun elemP tag p =  WSeq.$$["<", tag, ">"] && p && WSeq.$$["</", tag, ">"] 
        fun valueU v = elemU "value" v
        fun valueP p = elemP "value" p
        fun taggedValueU tag v = valueU (elemU tag v)
        fun taggedValueP tag p = valueP (elemP tag p) 
        fun param (toplevel:bool) value x = if toplevel then elemP "param" (value x) else value x
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
            in
                pu valueP unpickle 
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
        
        fun rpc a b {url, method} = 
            let 
                val hostP = ($ "http://" || success "") #-- getChars1 (fn #":" => false | #"/" => false | _ => true)
                val portP = getChars1 (fn #":" => true | _ => false) #-- scan(Int.scan StringCvt.DEC) || success 80
                val pathP = getChars0 (fn _ => true)
                val ((host, port), path) = 
                    let val opt = scanString (hostP -- portP -- pathP) url
                    in
                        case opt of
                            SOME i => i
                          | NONE => raise ServerConnection  "Malformed url"
                    end
                val  (pick,_) = a true
                val  (_, unpick)  = b true
                fun mkReq value =  
                    let val req = 
                        (WSeq.flatten (WSeq.$$ ["<?xml version=\"1.0\"?><methodCall><methodName>", method, "</methodName><params>"] 
                                       && (pick value) && (WSeq.$ "</params></methodCall>") && WSeq.Nl && WSeq.Nl)) 
                        val head =  WSeq.$$ ["POST ", path, " HTTP/1.0\n", "User-Agent: mlxmlrpc\n", "Host: ",  host, 
                                             "\nContent-Type: text/xml\n", "Content-Length: ", Int.toString (String.size req), "\n\n"]
                    in
                        (WSeq.flatten (head && (WSeq.$ req)))
                    end  
                
                fun unwrap answ = 
                    let (* val _ = X.log ("answer: " ^ answ) *)
                        val (header, methodResp) = Substring.position "<methodResponse>" (Substring.full(answ))
                        val (_, u) =  pair(int, string) false
                        fun fault (c, s) = raise MethodInvocation (c, s)
                        val faultU = elemU "fault" u >> fault  
                        val valueU = elemU "params" (elemU "param" unpick)
                        fun ckHead _ = let
                                           val p = "HTTP/1." $-- ($ "0 " || $ "1 ") #-- Parsercomb.scan(Int.scan StringCvt.DEC)
                                           val res = Parsercomb.scanSubstr p header
                                       in
                                           case res of 
                                               SOME i => if i = 200 then () else raise ServerConnection (Int.toString i)
                                             | NONE => raise ServerConnection "No wellformed answer recieved"
                                       end
                                   
                    in
                        ckHead();              
                        case Parsercomb.scanSubstr (elemU "methodResponse" (valueU || faultU)) methodResp of
                            SOME i => i 
                          | NONE => raise TypeConversion
                                handle Subscript => raise TypeConversion
                    end
            in
                (* FIXME should path be a part of the url (its already in the header) *)
                fn x => (unwrap (X.makeRequest url (mkReq x)))
                handle X.Connection str => raise ServerConnection str
            end     

        fun fault code str = 
            let val (intPickler, _) = int false
                val (strPickler, _) = string false
            in
                elemP "methodResponse" 
                      (elemP "fault" 
                             (taggedValueP "struct" 
                                           (elemP "member" 
                                                  (elemP "name" (WSeq.$ "faultCode")) && 
                                                  (intPickler code)) &&
                                           (elemP "member" 
                                                  (elemP "name" (WSeq.$ "faultString")) &&
                                                          (strPickler str))))
            end

        fun sfault n s = WSeq.flatten(fault n s)

        type method = {name: string, fnct: string -> string}

        fun method name au bp func : method = 
            let val (_, unpickle) = au false
                val (pickle, _) = bp true
                fun unpick str = 
                    let val ures = Parsercomb.scanString (elemU "params" (elemU "param" unpickle)) str
                    in case ures of 
                           SOME i => i
                         | NONE => raise TypeConversion
                    end
                fun res call = 
                    WSeq.flatten (elemP "methodResponse" (elemP "params" (pickle (func (unpick call)))))
                    handle TypeConversion => sfault 4 "Invalid parameter(s)"
                         | ex => sfault 5 ("Internal Error - The method raised: " 
                                           ^ (General.exnName ex)
                                           ^ " - with the following message: "
                                           ^ (General.exnMessage ex))
            in {name=name,fnct=res}
            end

        fun dispatch (ms:method list) : unit =
            let val req = X.getRequestData()
                (* val _ = X.log ("dispatch: req = " ^ req) *)
                val re = RegExp.fromString ".*<methodName>(.*)</methodName>.*(<params><param>.*</param></params>).*"
                val res = 
                    case RegExp.extract re req of
                        SOME [name,params] =>
                        let fun call [] = sfault 1 ("No method '" ^ name ^ "'")
                              | call (r::rest) = if #name r = name then #fnct r params
                                                 else call rest
                        in call ms
                        end
                      | _ => sfault 2 "Missing method name or wrong number of parameters"
            in X.write res
            end
            handle X.Connection str => raise ServerConnection str

    end (* local *)
end (* struct *)
