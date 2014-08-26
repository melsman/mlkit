(* File github.sml: Fetching data from github.
 * Copyright (c) 2014, Martin Elsman.
 * MIT License.
 *)

structure X = Js.XMLHttpRequest

fun get_info u =
    let val r = X.new()
    in X.openn r {method="GET", url=("https://api.github.com/users/" ^ u), 
                  async=false}
     ; X.send r NONE
     ; case X.response r of
          SOME s => s
        | NONE => raise Fail "no response"
    end

fun get u =
  let open Js.Element
  in Dojo.runDialog ("Github info for " ^ u) 
       (taga "textarea" [("readonly","true"),("style","width:600;height:600;")] ($(get_info u)))
  end
  
val () = get "melsman"
