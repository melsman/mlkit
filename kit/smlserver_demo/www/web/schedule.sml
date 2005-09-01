structure FV = FormVar

fun toint x = Option.getOpt (Option.map Int.fromString x, NONE)

fun optionapp f NONE = ()
  | optionapp f (SOME a) = f a

val first = toint (FV.wrapOpt FV.getStringErr "first")
val interval = toint (FV.wrapOpt FV.getStringErr "interval")
val script = FV.wrapOpt FV.getStringErr "script"
val kind = FV.wrapOpt FV.getStringErr "kind"

val _ = case kind of NONE => ()
                   | SOME("reg") => (
         case (first,interval,script) of
             (SOME(f), SOME(i), SOME(s)) => 
               Web.schedule s NONE 
                  (Date.fromTimeUniv(Time.+(Time.now(), Time.fromSeconds f))) 
                  (Time.fromSeconds i)
             | _ => ())
                   | SOME ("cancel") => optionapp Web.deSchedule script
                   | SOME _ => ()

val _ = Page.return "Schedule frontend"
`
<form method=post action=schedule.sml>
<table>
<td>First<td>Interval<td>Script<td>Action
<tr><td><input type=text size=10 name=first>
<td><input type=text size=10 name=interval>
<td><input type=text size=20 name=script>
<td><select name=kind>
<option selected value="reg">Reg</option>
<option value="cancel">Cancel</option></select>
<td><input type=submit> </tr>
</table>
</form>
`

