val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val _ = Db.execSp [`scs_user.imp_all_rows(always_p => 'f')`]

val _ = Ns.returnRedirect "imp_form.sml"
