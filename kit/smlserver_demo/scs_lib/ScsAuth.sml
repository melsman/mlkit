signature SCS_AUTH =
  sig
    (* [returnPg title body] returns a page to the client.*)
    val returnPg : string -> quot -> Ns.status
  end

structure ScsAuth :> SCS_AUTH =
  struct
    local
      val passwd_dict = [(ScsLang.da,`Skift kodeord`),(ScsLang.en,`Change password`)]
      val lang_dict = [(ScsLang.da,`Skift sprog`),(ScsLang.en,`Change language`)]
      val leftList = fn () => 
	[("/scs/auth/passwd_upd_form.sml", ScsDict.s passwd_dict),
	 ("/ucs/pvt/basic_info_form.sml", ScsDict.s lang_dict)]
      val rightList = fn () => []
    in
    fun returnPg title body =
      UcsPage.returnPgMenu "ucs@it.edu" "Niels Hallenberg" 
      title (leftList()) (rightList()) [] body
    end
  end