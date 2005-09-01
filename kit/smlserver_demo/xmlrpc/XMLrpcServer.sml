app load ["Int", "Substring", "Date"];
app use ["Parsercomb.sig", "Parsercomb.sml", "WSeq.sig", "WSeq.sml"];


structure XMLrpcServer :> XMLrpcServer =  struct
  exception TypeConversion
	  
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
			     
    local 
	fun elemU tag v =  
	    ($ ("<" ^ tag ^ ">") -- repeat0 ($ "\n"))  #-- v --# ($ ("</" ^ tag ^ ">") -- repeat0 ($ "\n"))
	fun elemP tag p =  WSeq.$$["<", tag, ">"] && p && WSeq.$$["</", tag, ">"] 
	fun valueU v =  elemU "value" v 
	fun valueP p = elemP "value" p
	fun taggedValueU tag v = valueU (elemU tag v)
	fun taggedValueP tag p = valueP (elemP tag p) 
	fun param toplevel value x = if toplevel then elemP "param" (value x) else value x
	fun pu p u = fn toplevel => (param toplevel p, u) 
    in 
        val int = 
	    let val unpick = 
		    let val legalInt = Parsercomb.scan(Int.scan StringCvt.DEC)
		    in	valueU (elemU "int" legalInt || elemU "i4" legalInt)
		    end
		val valueP = taggedValueP "int" o WSeq.$  o Int.toString
		val intPu = pu valueP unpick    
	    in 
		intPu
	    end
		
	val bool  = 
	    let val unpickle = taggedValueU "boolean" (Parsercomb.scan(Int.scan StringCvt.DEC) >> (fn x => x = 1))
		fun boolToStr b = if b then "1" else "0"
		val valueP =  taggedValueP "boolean"  o WSeq.$ o boolToStr
		val boolPu = pu valueP unpickle
	    in	
		boolPu
	    end 
		
	val real = 
	    let val unpickle = taggedValueU "double" (Parsercomb.scan  Real.scan)
		val valueP = taggedValueP "double"  o WSeq.$ o Real.toString
		val realPu = pu valueP unpickle 
	    in
		realPu
	    end
		
	val string = 
	    let val unpickle = 
		    let fun legalCh #"&" = false 
			  | legalCh #"<" = false 
			  | legalCh _ = true 
			val strConv = (getChar legalCh || ($ "&amp;" |> #"&") || ($ "&gt;" |> #">") || ($ "&lt;" |> #"<"))
			val strU = Parsercomb.repeat0 strConv >> String.implode
		    in
			valueU ((elemU "string" strU) || strU)
		    end
		fun strP #"<" = "&lt;" 
		  | strP #"&" = "&amp;"
		  | strP ch   = String.str(ch) 
		val valueP = taggedValueP "string" o WSeq.$ o String.translate strP
		val stringPu = pu valueP unpickle
	    in
		stringPu
	    end
		
	val date = 
	    let val unpickle =  
		    let fun getDate (((str, h), m), s) = 
			    let fun monthConv s = case s of 
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
						   in case v of SOME i => i 
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
		val datePu = pu valueP unpickle 
	    in
		datePu
	    end	      
		
	fun pair (a, b) =  
	    let val (apickle, aunpickle) = a false
		val (bpickle, bunpickle) = b false
		val unpickle = 
		    let val legalText = getChars0(fn #"<" => false | _ => true) 
			val name = elemU "name" legalText
			fun member p = elemU "member" (name -- p)
			fun validNames (a,b) = 
			    let val an = Int.fromString a
				val bn = Int.fromString b
			    in if (isSome an) andalso (isSome bn) then 
				   case Int.compare((valOf an), (valOf) bn) of
				       LESS => true 
				     | EQUAL => true 
				     | GREATER => if valOf bn + 1 = valOf an then false (* TODO  bn = 1 ? *) 
						  else true
			       else true
			    end
			val unpicka = fn ((aname, a),(bname,b)) => if validNames(aname,bname) then SOME (a,b) else NONE
			val unpickb = fn ((aname, a),(bname,b)) => if validNames(bname, aname) then SOME (b,a) else NONE 
			fun valStr par1 par2 =  taggedValueU  "struct" ((member par1) -- (member par2))
		    in 
			((valStr aunpickle bunpickle) >>* unpicka) ||  ((valStr bunpickle aunpickle) >>* unpickb)  
		    end 
		fun memberP name p = elemP "member" ((elemP "name" (WSeq.$ name)) && p) 
		val membersP = fn (x,y) => memberP "1" (apickle x) && memberP "2" (bpickle y)    
		val valueP = taggedValueP "struct" o membersP
		val pairUp = fn toplevel => if toplevel then (fn (x,y) => elemP "param" (apickle x && bpickle y), unpickle)
					    else (valueP, unpickle)
	    in
		pairUp
	    end
		
	fun list a =  
	    let val (pick, unpick) = a false
		val unpickle = taggedValueU  "array" (elemU "data" (repeat0 unpick))
		fun listpickler [] = WSeq.Empty |
		    listpickler (y::ys) = pick y && listpickler ys
		val valueP = taggedValueP  "array" o (elemP "data" o listpickler)
		val listPu = pu valueP unpickle 
	    in
		listPu
	    end
		
	fun vector a = 
	    let val (pick, unpick) = list a false
		fun toList v = Vector.foldr (fn (a,b) => a::b) [] v
		val pickle = pick o toList
		val unpickle =  unpick >> Vector.fromList
		val vectorPu = pu pickle unpickle
	    in
		vectorPu
	    end
		
	fun array a = 
	    let val (pick, unpick) = list a false
		fun toList a = Array.foldr (fn (a,b) => a::b) [] a
		val pickle = pick o toList
		val unpickle = unpick >> Array.fromList
		val arrayPu = pu pickle unpickle
	    in
		arrayPu
	    end
		
	fun export au bp func = 
	    let val (_, unpickle) = au false
		val (pickle, _) = bp true
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
		val unpick = fn str => 
				let  val ures = Parsercomb.scanString (elemU "params" (elemU "param" unpickle)) str
				in
				    case ures of 
					SOME i => i
				      | NONE => raise TypeConversion
				end
 		val res  = fn call => WSeq.flatten (elemP "methodResponse" (elemP "params" (pickle (func (unpick call)))))
			      handle TypeConversion => WSeq.flatten(fault ~1 "Invalid parameter(s)")
				   | ex => WSeq.flatten(fault ~1 ("Internal Error - The method raised: " 
								  ^ (General.exnName ex)
								  ^ " - with the following message: "
								  ^ (General.exnMessage ex)))
	    in 
		res
	    end    
    end (* local *)
end (* struct *)
