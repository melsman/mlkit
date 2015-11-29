(* Copyright 2015, Martin Elsman, MIT-license *)

(* Some utilities *)
fun removeChildren elem =
    case Js.firstChild elem of
        SOME c => (Js.removeChild elem c; removeChildren elem)
      | NONE => ()

fun setWindowOnload (f: unit -> unit) : unit =
    let open JsCore infix ==>
    in exec1{arg1=("a", unit ==> unit),
             stmt="return window.onload=a;",
             res=unit} f
    end
          
fun getElem id : Js.elem =
    case Js.getElementById Js.document id of
        SOME e => e
      | NONE => raise Fail "getElem"
                      
open Formlets
infixr 5 >> />
         
open Js.Element infix &

infix withLabel withKey withValue
          
fun sumbox() = fromEditCon(Dojo.textBox [("style","width:100%;")])

(* Ex 1 *)
val firstname = textbox() withKey "firstname"   withLabel "First Name"
val lastname = textbox()  withKey "lastname"    withLabel "Last Name"
val age = intbox()        withKey "age"         withLabel "Age"             withValue "0"
val male = boolbox()      withKey "male"        withLabel "Male"            withValue "true"
val tempc = realbox()     withKey "tempc"       withLabel "Temp in Celcius"
val tempf = sumbox()      withKey "tempf"       withLabel "Temp in Fahrenheit"
val tempk = realbox()     withKey "tempk"       withLabel "Temp in Kelvin"
                   
val but = button "Set age and Temp"
                 
val field1a = realbox()     withKey "field1a"       withLabel "Field 1A"
val field1b = realbox()     withKey "field1b"       withLabel "Field 1B"
val field1c = realbox()     withKey "field1c"       withLabel "Field 1C"
val field1sum = sumbox()    withKey "field1sum"     withLabel "Sum"
                       
val field2a = realbox()     withKey "field2a"       withLabel "Field 2A"
val field2b = realbox()     withKey "field2b"       withLabel "Field 2B"
val field2sum = sumbox()    withKey "field2sum"     withLabel "Sum"
                       
val subbut = button "Submit"
                    
val hid = hidden() withKey "hid"
                
val parent = tag0 "ul"
                  
val form = (group "Person" ((%firstname >> %lastname) /> (space >> space >> %age) /> (empty >> %male))
                  >> group "With a changer" (changer hid [("three", %field1a /> %field1b /> %field1c /> %field1sum),
                                                          ("two", %field2a /> %field2b /> %field2sum)]))
                 /> hextend(group "Temperature" (%tempc /> (%tempf >> %tempk) /> (hextend(group "ButGroup" (%%but)))) /> %%subbut /> group "Output" (elem parent))
                 
val initrule1 = init_rule (readonly tempf) (fn () => true)
val initrule2 = init_rule (readonly tempk) (fn () => true)
val initrule3 = init_rule (enabled age) (fn () => false)
val temprule1 = update_rule (value tempc) (value tempf) (fn c => case Real.fromString c of
                                                                     SOME c => Real.toString(9.0*c / 5.0 + 32.0)
                                                                   | NONE => "--")
val temprule2 = update_rule (value tempc) (value tempk) (fn c => case Real.fromString c of
                                                                     SOME c => Real.toString(c+273.15)
                                                                   | NONE => "--")
                            
val subrule = submit_rule subbut (fn kvs => (removeChildren parent;
                                             List.app (fn (k,v) => Js.appendChild parent (tag "li" ($(k ^ ":" ^ v))))) kvs)
                          
infix ||
          
val toggler =
    let val r = ref true
    in fn()=> if !r then "two" before r:=false
              else "three" before r:=true
    end  
        
val butrule = button_rule but emp (value age || value tempc || value hid) (fn () => (("23","40"),toggler()))
                          
val sumrule1 = update_rule (value field1a || value field1b || value field1c) (value field1sum) 
                           (fn ((a,b),c) => 
                               case (Real.fromString a, Real.fromString b, Real.fromString c) of
                                   (SOME a, SOME b, SOME c) => Real.toString(a+b+c)
                                 | _ => "--")
                           
val sumrule2 = update_rule (value field2a || value field2b) (value field2sum) 
                           (fn (a,b) => 
                               case (Real.fromString a, Real.fromString b) of
                                   (SOME a, SOME b) => Real.toString(a+b)
                                 | _ => "--")

val valrule = validate_rule (value tempc) (fn c => case Real.fromString c of
                                                       SOME c => if c >= ~273.15 then NONE
                                                                 else SOME "temp must be greater than -273.15"
                                                     | NONE => NONE)
                           
val formlet = (form, [initrule1,initrule2,initrule3,valrule,temprule1,temprule2,butrule,sumrule1,sumrule2,subrule])
                  
val () = print "<link rel='stylesheet' href='dojo/resources/dojo.css'>"
val () = print "<link rel='stylesheet' href='dgrid/css/dgrid.css'>"
val () = print "<link rel='stylesheet' href='dgrid/css/skins/claro.css'>"
val () = print "<link rel='stylesheet' href='dijit/themes/claro/claro.css'>"
val () = print "<link rel='stylesheet' href='mydojo.css'>"
val () = print "<body class='claro' id='body'></body>"
               
fun error s =
    Js.appendChild (getElem "body") (Js.Element.tag "p" (Js.Element.$s))

val () = setWindowOnload (fn () => install (getElem "body") formlet error)
