
val link_id = FormVar.wrapFail
  FormVar.getNatErr ("link_id", "Link id")

val delete =
  `delete from link
   where person_id = ^(Int.toString Login.person_id)
     and link_id = ^(Int.toString link_id)`

val _ = Db.dml delete

val _ = Ns.returnRedirect "/link/index.sml"
