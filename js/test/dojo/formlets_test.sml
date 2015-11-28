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

infix label
infix key
          
(* Ex 1 *)
val firstname = textbox() key "firstname"   label "First Name"
val lastname = textbox()  key "lastname"    label "Last Name"
val age = intbox()        key "age"         label "Age"
val male = boolbox()      key "male"        label "Male"
val tempc = realbox()     key "tempc"       label "Temp in Celcius"
val tempf = realbox()     key "tempf"       label "Temp in Fahrenheit"
val tempk = realbox()     key "tempk"       label "Temp in Kelvin"
                   
val but = button "Set age and Temp"
                 
val field1a = realbox()     key "field1a"       label "Field 1A"
val field1b = realbox()     key "field1b"       label "Field 1B"
val field1c = realbox()     key "field1c"       label "Field 1C"
val field1sum = realbox()   key "field1sum"     label "Sum"
                       
val field2a = realbox()     key "field2a"       label "Field 2A"
val field2b = realbox()     key "field2b"       label "Field 2B"
val field2sum = realbox()   key "field2sum"     label "Sum"
                       
val subbut = button "Submit"
                    
val hid = hidden() key "hid"
                
val parent = tag0 "ul"
                  
val form = group "Person" ((%firstname >> %lastname) /> (space >> space >> %age) /> (empty >> %male))
                 /> group "With a changer" (changer hid [("three", %field1a /> %field1b /> %field1c /> %field1sum),
                                                         ("two", %field2a /> %field2b /> %field2sum)])
                 /> group "Temperature" (%tempc /> (%tempf >> %tempk) /> (group "ButGroup" (%%but))) /> %%subbut /> group "Output" (elem parent)
                 
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
