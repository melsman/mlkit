
signature FREE_IDS =
  sig

    type longtycon and longstrid and strid and longid
    type funid and sigid
    type topdec and dec and strexp and sigexp

    type longids = {funids: funid list, sigids: sigid list,
		    longstrids: longstrid list, longtycons: longtycon list,
		    longvids: longid list}

    val fid_topdec : topdec -> longids
    val fid_strexp : strexp -> longids
    val fid_strexp_sigexp : strid -> strexp -> sigexp -> longids   (* in strexp, do not include
								    * longids with qualifier strid *)
    val fid_dec    : dec    -> longids

    type StringTree
    val layout_longids : longids -> StringTree

  end
