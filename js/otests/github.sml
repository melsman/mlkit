structure X = Js.XMLHttpRequest
fun report r i =
  if i > 2 then
     case X.response r of
       SOME s => print s
     | NONE => print "no response"
  else print "action"

fun get_info u =
    let val r = X.new()
    in X.openn r {method="GET", url=("https://api.github.com/users/" ^ u), 
                  async=false}
     ; X.onStateChange r (fn () => report r (X.state r))
     ; X.send r NONE
    end

val () = get_info "melsman"
