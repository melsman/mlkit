(* Copyright 2015, Martin Elsman, MIT-license *)

structure Formlets :> FORMLETS = struct
  type key = string
  type label = string
  type value = string

  (* Some utilities *)
  fun qq s = "'" ^ s ^ "'"
  fun die s = raise Fail ("Formlets die: " ^ s)
  fun removeChildren elem =
      case Js.firstChild elem of
          SOME c => (Js.removeChild elem c; removeChildren elem)
        | NONE => ()

  open Js.Element infix &

  (* Elements *)
  type hidden = {value:string ref,listeners:(string -> unit)list ref}
  datatype elem = EDITCON of string Dojo.editCon | BUTTON | HIDDEN of hidden
  type el = {elem:elem,key:string,label:string,id:int}

  val newId : unit -> int =
      let val c = ref 0
      in fn () => !c before c:= !c + 1
      end
      
  fun fromEditCon (ec: string Dojo.editCon) : el = {elem=EDITCON ec,key="",label="",id=newId()}
  val textbox : unit -> el = fn () => fromEditCon (Dojo.textBox[])
  val intbox  : unit -> el = fn () => fromEditCon (Dojo.textBox[])
  val realbox : unit -> el = fn () => fromEditCon (Dojo.textBox[])
  val datebox : unit -> el = fn () => fromEditCon (Dojo.textBox[])
  fun selectbox sls : el = fromEditCon (Dojo.filterSelectBox[] (List.map (fn (k,v) => {id=k,name=v}) sls))
  val boolbox : unit -> el = fn () => selectbox [("true","True"),("false","False")]
  val hidden : unit -> el = fn () => {elem=HIDDEN {value=ref "",listeners=ref nil},key="",label="",id=newId()}

  fun key (e:el,k) : el = 
      if #key e = "" then {elem= #elem e,label= #label e,key=k,id= #id e}
      else die ("Cannot set key " ^ qq k ^ " for an element that already has the key " ^ qq(#key e))

  fun label (e:el,l) : el = {elem= #elem e,label=l,key= #key e,id= #id e}

  type button = el
  val button : label -> el = fn label => {elem=BUTTON,key="",label=label,id=newId()}

  (* Forms *)
  datatype form = Lf of el                              (* Leaf *)
                | Vf of form list                       (* Vertical *)
                | Hf of form list                       (* Horizontal *)
                | Gf of label * form                    (* Group with label *)
                | Ef                                    (* Empty (identity for >> and />) *)
                | Elf of Js.elem                        (* Dom element *)
                | Cf of el * (string*form)list          (* Changer *)

  val % : el -> form = fn x => Lf x
  val %% : button -> form = %

  fun op >> (Ef,f) = f
    | op >> (f,Ef) = f
    | op >> (Hf fs1, Hf fs2) = Hf(fs1@fs2)
    | op >> (Hf fs1, f) = Hf(fs1@[f])
    | op >> (f,Hf fs2) = Hf(f::fs2)
    | op >> (f1,f2) = Hf[f1,f2]

  fun op /> (Ef,f) = f
    | op /> (f,Ef) = f
    | op /> (Vf fs1, Vf fs2) = Vf(fs1@fs2)
    | op /> (Vf fs1, f) = Vf(fs1@[f])
    | op /> (f,Vf fs2) = Vf(f::fs2)
    | op /> (f1,f2) = Vf[f1,f2]

  val group     : label -> form -> form = fn l => fn f => Gf(l,f)
  val empty     : form = Ef
  val changer   : el -> (string * form) list -> form = fn el => fn sfs => Cf(el,sfs)
  val space     : form = Elf($"")
  val elem      : Js.elem -> form = Elf

  (* Fields *)
  datatype f0 = value0 of el | readonly0 of el | enabled0 of el | pair0 of f0 * f0 | emp0
  datatype gen = Sgen of string | Ugen | Pgen of gen * gen | Bgen of bool
  fun unPgen (Pgen (g1,g2)) = (g1,g2)
    | unPgen _ = die "unPgen"
  fun unSgen (Sgen s) = s
    | unSgen _ = die "unSgen"
  fun unBgen (Bgen b) = b
    | unBgen _ = die "unBgen"
  type 'a f = f0 * ('a -> gen) * (gen -> 'a)

  fun value e0 = (value0 e0,Sgen,unSgen)
  fun readonly e0 = (readonly0 e0,Bgen,unBgen)
  fun enabled e0 = (enabled0 e0,Bgen,unBgen)
  fun || ((f01,to1,from1),(f02,to2,from2)) = (pair0(f01,f02), fn(x,y)=>Pgen(to1 x,to2 y), fn g => let val (x,y) = unPgen g in (from1 x, from2 y) end)
  val emp = (emp0,fn () => Ugen, fn Ugen => () | _ => die "unUgen")

  (* Rules *)
  exception FormletError of string
  datatype rule = Init_rule of f0 * (unit -> gen) | Update_rule of el option * f0 * f0 * (gen -> gen) | Submit_rule of el * ((key*value)list -> unit) | Load_rule of unit -> (key*value)list
  fun init_rule (f : 'a f) (g: unit -> 'a) : rule = Init_rule (#1 f, #2 f o g)
  fun load_rule f : rule = Load_rule f
  fun update_rule (f1: 'a f) (f2: 'b f) (g: 'a -> 'b) : rule = Update_rule (NONE, #1 f1, #1 f2, #2 f2 o g o #3 f1)
  fun button_rule (e:el) (f1: 'a f) (f2: 'b f) (g: 'a -> 'b) : rule = Update_rule (SOME e, #1 f1, #1 f2, #2 f2 o g o #3 f1)
  fun validate_rule (f:'a f) (g: 'a -> string option) : rule =
      update_rule f emp (fn x => case g x of NONE => () 
                                           | SOME s => raise FormletError s)
  fun submit_rule (e:el) f = Submit_rule(e,f)
 
  (* Interpretation *)
  val ret = Dojo.ret
  val >>= = Dojo.>>= infix >>=
  type 'a M = 'a Dojo.M
  type formlet = form * rule list

  fun tag_sty t s e = taga t [("style",s)] e

  fun boxGroup lab e =
    Dojo.pane [("style","height:auto;")] e >>= (fn p =>
    Dojo.titlePane [("title",lab)] p >>= (fn w => 
    (Dojo.setBoolProperty ("toggleable", false) w;
     let val parent = taga0 "td" [("style","vertical-align: top;")]
     in Dojo.attachToElement parent (Dojo.ret w) (fn () => ())
      ; Dojo.ret parent
     end)))

  datatype key_thing = ED of string Dojo.Editor.t | BUT of Dojo.Button.t * ((unit->unit)->unit) | HID of hidden * (string -> unit)

  (* Assumptions:
    1. No duplicate use of elements
    2. No duplicate use of keys
  *)
  fun mkForm form : ((int*key*key_thing)list * Js.elem) M =         (* invariant: returns a list of key mappings and an element representing row content *)
      case form of
          Lf {elem=BUTTON,key,label,id} =>
          let val listeners : (unit->unit) list ref = ref nil
              fun onclick () = List.app (fn f => f()) (!listeners)
              fun attachOnclick f = listeners := (f :: !listeners)
          in Dojo.Button.mk [("label",label)] onclick >>= (fn but =>
             let val e = Dojo.Button.domNode but
             in ret ([(id,key,BUT(but,attachOnclick))],tag "td" e)
             end)
          end
        | Lf {elem=EDITCON ec,key,label,id} =>
          Dojo.Editor.mk ec >>= (fn ed =>
          let val e = tag "td" (Dojo.Editor.domNode ed)
              val e = if label <> "" then tag "td" ($label) & e else e
          in ret ([(id,key,ED ed)], e)
          end)
        | Lf {elem=HIDDEN vl,key,label,id} => ret ([(id,key,HID (vl,fn _ => ()))], $"")
        | Vf forms => 
          mkForms forms >>= (fn (kvs,es) =>
          let val trs = List.foldr (fn (e,a) => tag "tr" e & a) ($"") es
          in ret(kvs, tag "td" (tag "table" trs))
          end)
        | Hf forms => 
          mkForms forms >>= (fn (kvs,es) =>
          let val e = List.foldr (fn (e,a) => e & a) ($"") es
          in ret(kvs, e)
          end)
        | Gf (lab,form) =>
          mkForm form >>= (fn (kvs,e) => 
          boxGroup lab (tag "table" (tag "tr" e)) >>= (fn e =>
          ret (kvs,e)))
        | Ef => ret (nil, $"")
        | Elf e => ret (nil, tag "td" e)
        | Cf({elem=HIDDEN vl,key,id,...},sfs) =>
          mkKeyForms sfs >>= (fn (kvs,ses) =>
          case ses of
              (s,e)::_ => 
              let val e0 = tag "div" e
                  fun find nil _ = NONE
                    | find ((k,v)::kvs) s = if k=s then SOME v else find kvs s 
                  fun onChange s =
                      case find ses s of
                          SOME e => (removeChildren e0;
                                     Js.appendChild e0 e)
                        | NONE => die ("changer: cannot find element for " ^ s)
              in #value vl := s
               ; ret ((id,key,HID (vl,onChange))::kvs,e0)
              end
            | _ => die "Changer requires at least one possibility"
          )
        | Cf _ => die "Changer requires hidden field"

  and mkForms nil : ((int*key*key_thing)list * Js.elem list) M = ret (nil,nil)
    | mkForms (form::forms) = mkForm form >>= (fn (kvs1,e) =>
                              mkForms forms >>= (fn (kvs2,es) => ret (kvs1@kvs2,e::es)))
  and mkKeyForms nil : ((int*key*key_thing)list * (string*Js.elem) list) M = ret (nil,nil)
    | mkKeyForms ((s,form)::forms) = mkForm form >>= (fn (kvs1,e) =>
                                     mkKeyForms forms >>= (fn (kvs2,es) => ret (kvs1@kvs2,(s,e)::es)))

  structure Rules = struct
    fun lookup nil id = NONE
      | lookup ((id0,_,ed)::rest) id = if id=id0 then SOME ed else lookup rest id

    fun lookup_key nil key = NONE
      | lookup_key ((_,k,ed)::rest) key = if k=key then SOME ed else lookup_key rest key

    fun upd_key kvs (k,v) =
        case lookup_key kvs k of
            SOME (ED ed) => Dojo.Editor.setValue ed v
          | SOME (HID(vl,_)) => #value vl := v
          | SOME (BUT _) => die ("upd_key.does not expect button for key " ^ qq k)
          | NONE => die ("upd_key.no editor for key " ^ qq k)

    fun upd kvs f0 g =
        case f0 of
            value0 {id,...} =>
            (case lookup kvs id of
                 SOME (ED ed) => Dojo.Editor.setValue ed (unSgen g)
               | SOME (HID(vl,onChange)) =>
                 let val s = unSgen g
                 in #value vl := s
                  ; List.app (fn f => f s) (!(#listeners vl))
                  ; onChange s
                 end
               | SOME (BUT _) => die "Rules.upd.button"
               | NONE => die ("Rules.upd." ^ Int.toString id))
          | readonly0 {id,...} =>
            (case lookup kvs id of
                 SOME (ED ed) => Dojo.Editor.setReadOnly ed (unBgen g)
               | SOME (HID _) => die "Rules.upd.readonly.hidden"
               | SOME (BUT _) => die "Rules.upd.readonly.button"
               | NONE => die ("Rules.upd.readonly" ^ Int.toString id))
          | enabled0 {id,...} =>
            (case lookup kvs id of
                 SOME (ED ed) => Dojo.Editor.setDisabled ed (not(unBgen g))
               | SOME (HID _) => die "Rules.upd.enabled.hidden"
               | SOME (BUT _) => die "Rules.upd.enabled.button"
               | NONE => die ("Rules.upd.enabled." ^ Int.toString id))
          | emp0 => ()
          | pair0 (f01,f02) => 
            let val (g1,g2) = unPgen g
            in upd kvs f01 g1;
               upd kvs f02 g2
            end
                
    fun get kvs f0 : gen =
        case f0 of
            value0 {id,...} =>
            (case lookup kvs id of
                 SOME (ED ed) => Sgen(Dojo.Editor.getValue ed)
               | SOME (HID (vl,_)) => Sgen(!(#value vl))
               | SOME (BUT _) => die "Rules.get.button"
               | NONE => die ("Rules.get." ^ Int.toString id))
          | readonly0 {id,...} => die "Rules.get.readonly not implemented"
          | enabled0 {id,...} => die "Rules.get.enabled not implemented"
          | emp0 => Ugen
          | pair0 (f01,f02) => Pgen(get kvs f01, get kvs f02)

    fun inst kvs f0 f : unit =
        case f0 of
            value0 {id,...} =>
            (case lookup kvs id of
                 SOME (ED ed) => Dojo.Editor.onChange ed (fn _ => f())
               | SOME (HID (vl,_)) => (#listeners vl) := (fn _ => f()) :: (!(#listeners vl))
               | SOME (BUT _) => die "Rules.inst.button"
               | NONE => die ("Rules.inst." ^ Int.toString id))
          | readonly0 {id,...} => die "Rules.inst.readonly not implemented"
          | enabled0 {id,...} => die "Rules.inst.enabled not implemented"
          | emp0 => ()
          | pair0 (f01,f02) => (inst kvs f01 f; inst kvs f02 f)

    fun getValues kvs =
        List.foldl (fn ((_,"",_),a) => a
                     | ((_,_,BUT _),a) => a
                     | ((_,k,ED ed),a) => (k,Dojo.Editor.getValue ed)::a
                     | ((_,k,HID (vl,_)),a) => (k, !(#value vl))::a) nil kvs

    fun setupRule error_reporter dojo_form kvs r =
        case r of
            Init_rule (f0,f:unit -> gen) => upd kvs f0 (f())
          | Update_rule (NONE,f01,f02,f) =>
            let fun onchange () =
                    upd kvs f02 (f(get kvs f01))
                    handle FormletError s => (error_reporter s; raise Fail "formlet error")
            in inst kvs f01 onchange
            end
          | Update_rule (SOME {id,...},f01,f02,f) =>
            (case lookup kvs id of
                 SOME (BUT (_,attachOnClick)) => attachOnClick (fn () => upd kvs f02 (f(get kvs f01)))
               | SOME (HID _) => die ("Rules.setupRule.expecting button - got hidden for " ^ Int.toString id)
               | SOME (ED ed) => die ("Rules.setupRule.expecting button - got ed for " ^ Int.toString id)
               | NONE => die ("Rules.setupRule.expecting button - got nothing for " ^ Int.toString id))
          | Submit_rule ({id,...},f) => 
            (case lookup kvs id of
                 SOME (BUT (_,attachOnClick)) => attachOnClick (fn () => if Dojo.Form.validate dojo_form then (f(getValues kvs); Dojo.Form.startup dojo_form)
                                                                         else f nil)
               | SOME (HID _) => die ("Rules.setupRule.submit.expecting button - got hidden for " ^ Int.toString id)
               | SOME (ED ed) => die ("Rules.setupRule.submit.expecting button - got ed for " ^ Int.toString id)
               | NONE => die ("Rules.setupRule.submit.expecting button - got nothing for " ^ Int.toString id))
          | Load_rule f => List.app (upd_key kvs) (f())
  end                                                      

  type error_reporter = string -> unit

  (* At formlet construction time, we should check, early, for a number of properties:
       1. no cycles in update rule graph
       2. no cycles in button rules (why is this a problem?)
       3. all rule field ids are present in the form
       4. no element key is overwritten (done)
       5. an element key is defined at most once
       6. elements (their ids) are used at most once in a form
  *)
  fun mk (form,rules) error_reporter : ((unit->unit) * Dojo.widget) M =
      mkForm form >>= (fn (kvs,e) =>
      Dojo.Form.mk[] >>= (fn dojo_form =>
      let val form_elem = Dojo.Form.domNode dojo_form
          val () = Js.appendChild form_elem (tag "table" (tag "tr" e))
          fun startup () =
              (List.app (fn (_,_,ED ed) => Dojo.Editor.startup ed
                        | _ => ()) kvs;
               Dojo.Form.startup dojo_form; Js.setStyle form_elem ("height","auto;"))
      in Dojo.pane [("style","height:100%;overflow:auto;")] form_elem >>= (fn w =>
         (List.app (Rules.setupRule error_reporter dojo_form kvs) rules;
          ret (startup,w)))
      end))

  fun install (e: Js.elem) formlet error_reporter : unit =
      let val startupRef = ref (fn()=>())
          val wM = mk formlet error_reporter >>= (fn (startup,w) =>
                   (startupRef := startup;
                    ret w))
      in Dojo.attachToElement e wM (!startupRef)
      end

end
