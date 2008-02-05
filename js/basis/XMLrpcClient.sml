
local
  fun makeRequest {url,request} : string =
      let val r = Js.XMLHttpRequest.new()
          val _ = Js.XMLHttpRequest.openn r {method="POST",url=url,sync=true}
          val _ = Js.XMLHttpRequest.send r (SOME request)
      in case Js.XMLHttpRequest.response r of
           SOME s => s
         | NONE => raise Fail ("makeRequest.no response; state=" ^ 
                               Int.toString (Js.XMLHttpRequest.state r))
      end

  fun makeRequestAsync {url,request,cont} : unit =
      let val r = Js.XMLHttpRequest.new()
          val _ = Js.XMLHttpRequest.openn r {method="POST",url=url,sync=false}
          val _ = Js.XMLHttpRequest.onStateChange r 
                     (fn() =>
                        if Js.XMLHttpRequest.state r > 3 then
                          if Js.XMLHttpRequest.status r = SOME 200 then
                            (case Js.XMLHttpRequest.response r of
                               SOME res => cont res
                             | NONE => raise Fail "makeRequestAsync.no response text")
                          else raise Fail "makeRequestAsync.status not 200"
                        else ())
      in Js.XMLHttpRequest.send r (SOME request)
      end
in
structure XMLrpc = 
  XMLrpc(struct 
           exception Connection = Fail
           val makeRequest = makeRequest
           val makeRequestAsync = makeRequestAsync
         end)
end
