val _ = (ScsPage.returnTop "Audit" ;
(*  (ScsAudit.trail ["udtraeksdato"] NONE NONE "tfs" [("id","5226")]) *)
	 ScsPage.write `<blockquote>`;
	 (ScsAudit.trail_for_table ["udtraeksdato"] (SOME (ScsDate.genDate(29,11,2001))) NONE "stu" ["id"]);
	 ScsPage.write `</blockquote>`;
	 ScsPage.returnBot())

(*  (ScsAudit.trail_for_table ["udtraeksdato"] (SOME (ScsDate.genDate(29,11,2001))) NONE "tfs" ["person_lnr","hold","semester"])*)
