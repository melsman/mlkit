val email = ScsFormVar.wrapFail ScsFormVar.getEmailErr ("email","Email")

val (passwd,first_names,last_name) =
  ScsDb.oneRowErrPg' (fn g => (g "password", g "first_names", g "last_name"),
		      `select password,first_names,last_name
                         from scs_persons, scs_users, scs_parties
		        where scs_parties.email = ^(Db.qqq email)
                          and scs_parties.party_id = scs_persons.person_id
                          and scs_persons.person_id = scs_users.user_id
                          and scs_parties.deleted_p = 'f'
                          and scs_persons.deleted_p = 'f'
                          and scs_users.deleted_p = 'f'`,
		      `You typed an email that is not in our database - 
			    please click your browsers back button and try again.<p>`)

val _ = 
  (Ns.Mail.send {to=email, from=ScsConfig.scs_site_adm_email(),subject="Obtain Password",
		 body=Quot.toString `Dear ^first_names ^last_name

You can access ^(ScsConfig.scs_site_name()) from

  ^(ScsConfig.scs_site_url())

  Username: ^email
  Password: ^passwd

Best Regards,

^(ScsConfig.scs_site_name())`};
   ScsPage.returnPg "Password has been mailed" 
     `In a short time, you'll receive an email with your password.<p>

      <a href="^(ScsConfig.scs_site_index_page())">Go to the main page</a>`)


