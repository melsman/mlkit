
structure OAuth : OAUTH = struct

(* utils *)

fun keyvalues (ss:string list) : (string*string) list =
    map (fn s => case String.tokens (fn c => c = #"=") s of
                     [k,v] => (k,v)
                   | _ => raise Fail ("OAuth.keyvalues: " ^ s)) ss

fun look x [] = NONE
  | look x ((k,v)::rest) = if x = k then SOME v else look x rest

(* cookies *)

fun setCookie (k:string) (v:string) : unit =
    Js.setCookie Js.document (k ^ "=" ^ v)

fun deleteCookie (k:string) : unit =
    Js.setCookie Js.document (k ^ "= ; expires = Thu, 01 Jan 1970 00:00:00 GMT")

fun getCookie (k:string) : string option =
    let val cookies = String.tokens (fn c => c = #";") (Js.getCookie Js.document)
        val pairs = keyvalues cookies
    in look k pairs
    end

val oauth_access_token_key = "oauth_access_token"
val oauth_state_key = "oauth_state"

(* requests *)

local structure X = Js.XMLHttpRequest
in fun getRequest url =
       let val r = X.new()
           val () = X.openn r {method="GET",url=url,async=false}
           val () = X.send r NONE
       in case X.response r of
              SOME res => res
            | NONE => raise Fail ("dropbox.getRequest failed: " ^ url)
       end
end

type client = {authorize    : string,
               client_id    : string,
               redirect_uri : string
              }

type token = string

fun setLocation (url:string) : unit =
    JsCore.exec1 {stmt="return (window.location = url);", arg1=("url", JsCore.string),
                  res=JsCore.unit} url

fun getLocation () : string =
    JsCore.exec0 {stmt="return window.location.href;",
                  res=JsCore.string} ()

fun client (x : client) = x

fun token (c:client) : token option =
    case getCookie oauth_access_token_key of
        SOME t => SOME t
      | NONE =>
        (case String.tokens (fn c => c = #"#") (getLocation()) of
             [_,args_string] =>
             let val args : string list = String.tokens (fn c => c = #"&") args_string
                 val arg_pairs = keyvalues args
                 fun check key expected =
                     case look key arg_pairs of
                         SOME s => if s = expected then ()
                                   else raise Fail ("OAuth.token: Expecting " ^ key ^ " arg "
                                                    ^ expected ^ " - got " ^ s)
                       | NONE => raise Fail ("OAuth.token: Expecting a " ^ key ^ " parameter")
                 val () = case getCookie oauth_state_key of
                              SOME state => check "state" state
                            | NONE => raise Fail ("OAuth.token: Expecting state cookie")
                 val () = check "token_type" "bearer"
                 val t = case look "access_token" arg_pairs of
                             SOME t => t
                           | NONE => raise Fail ("OAuth.token: Expecting access_token")
                 val () = setCookie oauth_access_token_key t
                 val () = deleteCookie oauth_state_key
             in SOME t
             end
           | _ => NONE)
        handle Fail msg => NONE

fun authorize ({authorize,client_id,redirect_uri}:client) : unit =
    let infix *>
        fun a *> b = (a,b)
        fun pr_arg (k,v) = k ^ "=" ^ v

        val state = Time.toString(Time.now()) (* maybe hash it... *)
        val args = ["client_id" *> client_id,
                    "state" *> state,
                    "redirect_uri" *> redirect_uri,
                    "response_type" *> "token"]
        val newloc = authorize ^ "?" ^
                     String.concatWith "&" (map pr_arg args)
    in setCookie oauth_state_key state
     ; setLocation newloc
    end

fun logout ({redirect_uri,...}:client) : unit =
    ( deleteCookie oauth_access_token_key
    ; deleteCookie oauth_state_key
    ; setLocation redirect_uri
    )

end
