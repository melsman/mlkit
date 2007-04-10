functor WebLowMail (type conn = foreignptr 
                   val getReqRec : unit -> conn 
                   val log : string -> unit 
                   structure Info : WEB_INFO
                   exception Forbidden
                   ) :> WEB_LOW_MAIL = 
struct
  exception ConnectionErr of (string * (string * string) list * 
                              (string * string) list * (string * string) list);
  exception connErr of (string * (int * string) list * 
                              (int * string) list * (int * string) list);
  type MX_FQDN = string
  type mailer = int option ref;
  fun getFQDN_MX (name : string) = 
    let val fqdnlist : (int * int * MX_FQDN) list = prim("apdns_getFQDN_MX", (name, getReqRec()))
    in Listsort.sort (fn ((i1,_,_), (i2,_,_)) => Int.compare(i1,i2)) fqdnlist 
    end
  fun FQDN_MX_toString (s : MX_FQDN) = s : string
  fun FQDN_MX_fromString (s : string) = s : MX_FQDN
  val FQDN_MX_compare = String.compare
  fun getErrNr (i : int) : string = prim("apsml_errnoToString", i)

  fun parseErr (i,j) = case i of 0 => "OK"
                         | 1 => "A system call failed: " ^ getErrNr(j)
                         | 2 => "We got a timeout"
                         | 3 => "We got a protocol failure"
                         | 4 => "We got an internal error: " ^ Int.toString j
                         | 5 => "The connection was closed"
                         | 6 => "We got a service error: " ^ Int.toString j ^" from the other end"
                         | 7 => "Only part of the email was received by the server"
                         | _ => "No mailer created"

  fun getErr (mail : int) = let val a : (int * int) = prim("apsml_mailGetError", (mail))
                  in parseErr(a) 
                  end

  fun getDefaultServer () : MX_FQDN option = Info.configGetValue (Info.Type.String, "MailRelay")

  fun closeConn (mail' : mailer) = 
            case mail' of ref NONE => raise Forbidden
                        | ref (SOME(mail)) =>
              let 
                val _ = log "prim apsml_closeconn"
                val _ = prim("apsml_closeconn", (mail, getReqRec())) : unit
              in (log "prim apsml_closeconn DONE"; ()) 
              end

  fun initConn (server : string) : mailer = (
    let fun getvalue s = getOpt (Info.configGetValue (Info.Type.Int, s), 0)
        val toInit : int = getvalue "MailTimeOutInit"
        val toEhlo : int = getvalue "MailTimeOutEhlo"
        val toMail : int = getvalue "MailTimeOutMail"
        val toRcpt : int = getvalue "MailTimeOutRcpt"
        val toData : int = getvalue "MailTimeOutData"
        val toDataB : int = getvalue "MailTimeOutDataBlock"
        val toDataT : int = getvalue "MailTimeOutDataTerm"
        val mail : int =  prim("apsml_mailer_initconn", 
                (server,(toInit,toEhlo,toMail,toRcpt,toData,toDataB,toDataT), getReqRec()))
        val _ : int = prim("apsml_mailer_initconnCheckCon", (mail, getReqRec()))
        handle Overflow => (
            let val err = getErr(mail)
                val _ = if mail <> 0 then closeConn(ref (SOME(mail))) else ()
            in 
            raise ConnectionErr (err, [],[],[])
            end)
     in if mail = 0 
        then raise ConnectionErr("Out of mem",[],[],[]) 
        else ref (SOME(mail))
     end)

   fun mysendmail (to, from : string , data : string, mail' : mailer) = 
       case mail' of ref NONE => raise Forbidden
                   | ref (SOME(mail)) => 
         let 
         fun chop max li acc = if List.length li > max 
                               then chop max (List.drop (li, max)) ((List.take (li, max),from)::acc) 
                               else (li, from) ::acc
         fun myzip [] _ = []
           | myzip (x::xr) msg = 
               let fun myzip' ([],_) = []
                     | myzip' ((a,_)::r,b) = (a,msg)::myzip'(r,b)
               in (myzip xr msg)@ myzip' x
               end
         fun mysendmail' ((tolist : (int * string) list, from : string), (ok, tmp, perm)) =
             let 
                 val _ = log "prim apsml_sendmail"
                 val res : int = 
                     prim("apsml_sendmail", (tolist, List.length tolist, from, data, mail))
                     handle Overflow =>
                     ( let val _ = log("closeConn 74")
                           val _ = closeConn(mail')
                       in raise connErr ("Memory Error", ok, tmp, 
                                myzip [(tolist,from)] "Memory Error" @ perm)
                       end
                     )
                 val _ = log("apsml_sendmail res: " ^Int.toString res)
                 val okeyed : (int * string) list = prim("apsml_mailget", (0,mail))
                 val tempfail : (int * string) list = prim("apsml_mailget", (1,mail))
                 val permfail : (int * string) list = prim("apsml_mailget", (2,mail))
             in if res < 0
                then let 
                         val (etype, err) : (int * int) = prim("apsml_mailGetError", (mail))
                         val _ = log "closeConn 86"
                         val _ = closeConn(mail');
                     in raise connErr(parseErr(etype,err) , 
                                  okeyed @ ok, tempfail @ tmp, permfail @ perm)
                     end
                else (okeyed @ ok, tempfail @ tmp, permfail @ perm)
             end
         fun myfoldr f bot li =
             let fun myfoldr' f [] acc = acc
                   | myfoldr' f (x::xr) (acc1,acc2,_) = 
                       let val t = (f(x,acc1),acc2,NONE)
                             handle connErr(err,ok,tmp,perm) => 
                             ((ok, tmp, perm),xr,SOME(err))
                       in 
                         case t of (_,_,NONE) => myfoldr' f xr t
                                 |  _ => t
                       end
              in myfoldr' f li (bot,[],NONE)
              end
         val ((ok,tmp,perm),b,err) = myfoldr mysendmail' ([], [], []) (chop 99 to []) 
     in case err of NONE => (ok,tmp, perm) 
                  | SOME(err') => raise connErr(err', ok, tmp, myzip b err' @ perm)
     end 

     
   fun sendmail (to,from,data,mail) = 
         let
         val mailno = ref 1;
         val (inttomail,sendlist) = foldl 
                               (fn (x,(dict,l))=> (mailno:= !mailno+1;
                                  (Binarymap.insert(dict, !mailno, x), (!mailno,x)::l))) 
                               (Binarymap.mkDict Int.compare,[]) to
         fun inttostring li = List.map (fn (x,y) => (Binarymap.find(inttomail, x),y)) li
         val (ok,tmp,perm) = mysendmail(sendlist,from,data,mail) 
                             handle connErr(a,b,c,d) => 
                             raise ConnectionErr(a,inttostring(b),inttostring(c),inttostring(d))
         in (inttostring(ok),inttostring(tmp),inttostring(perm))
         end
      

end
