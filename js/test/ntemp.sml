val tf_f = Web.divintfield (fn _ => ())
fun comp (i:int) = 
    let val t_f = 9 * i div 5 + 32
    in #write tf_f t_f
    end
val tf_c = Web.intfield comp

local open Html infix &&
in
val page = html(body(h1($"Temperature Conversion") &&
               table (tr(th($"Temperature in Celcius:") && td(#html tf_c)) &&
                        tr(th($"Temperature in Fahrenheit:") && td(#html tf_f)))))

val () = printhtml page
val () = #action tf_c ()
end
