local

(*
  (* RegExp SML/NJ Utilities *)
  local
    ../sml/RegExp/Utils/ord-key-sig.sml
    ../sml/RegExp/Utils/lib-base-sig.sml
    ../sml/RegExp/Utils/lib-base.sml
    ../sml/RegExp/Utils/ord-set-sig.sml
    ../sml/RegExp/Utils/list-set-fn.sml
    ../sml/RegExp/FrontEnd/syntax-sig.sml
    ../sml/RegExp/FrontEnd/syntax.sml
    ../sml/RegExp/FrontEnd/parser-sig.sml
    ../sml/RegExp/Glue/match-tree.sml
    ../sml/RegExp/BackEnd/engine-sig.sml
    ../sml/RegExp/Glue/regexp-sig.sml
    ../sml/RegExp/Glue/regexp-fn.sml
    ../sml/RegExp/Utils/ord-map-sig.sml
    ../sml/RegExp/Utils/list-map-fn.sml
    ../sml/RegExp/BackEnd/fsm.sml
    (*../sml/RegExp/BackEnd/dfa-engine.sml*)
    ../sml/RegExp/BackEnd/bt-engine.sml
    ../sml/RegExp/FrontEnd/awk-syntax.sml
  in
    ../sml/RegExp/REG_EXP.sml
    ../sml/RegExp/RegExp.sml
  end

  ../sml/RegExp2.sml
*)

  ../sml/Quot.sml

  local
    ../sml/NsBasics.sml
    ../sml/NS_SET.sml
    ../sml/NsSet.sml
    ../sml/NsInfo.sml
    ../sml/NS_DB.sml
    ../sml/DbFunctor.sml
  in
    ../sml/NS.sml
    ../sml/Ns.sml  
    ../sml/Db.sml
  end
  ../sml/SmlsLang.sml
  ../sml/SmlsLogin.sml
  ../sml/SmlsDate.sml
  ../sml/SmlsList.sml
  ../sml/SmlsDict.sml
  ../sml/MSP.sml
  ../sml/Msp.sml
  ../sml/FormVar.sml
  ../sml/RatingUtil.sml


in
 [
  rating/index.sml
  rating/add.sml
  rating/add0.sml
  rating/wine.sml
  employee/update.sml
  employee/search.sml
  time_of_day.sml
  cache.sml
  cache_lookup.sml
  cache_add.sml
  guess.sml
  counter.sml
  temp.sml
  recipe.sml
  hej.sml
  yellow.sml
  hello.sml
  show.sml
  h1.sml
  hello.msp
  calendar.msp
  test.msp
  index.msp
  logtofile.msp
  fileindex.msp
  dir.msp
  friends.msp
  friends_add_form.msp
  friends_add.msp
  server.sml
  mail_form.sml
  mail.sml
  mul.msp

  cs_form.sml
  cs_add.sml
  cs_upd.sml
  cs_const.sml

  ug.sml
  currency.sml
  currency_cache.sml
  regexp.sml
  cookie.sml
  cookie_set.sml
  cookie_delete.sml

  formvar.sml
  formvar_chk.sml
  url_desc.sml

  auth_example.sml
  auth.sml
  auth_form.sml
  auth_logout.sml
  show_cookies.sml

  email_form.sml
  email_sent.sml
 ]
end
