
signature FREE_IDS =
  sig

    type longtycon and longstrid and longid
    type funid and sigid
    type topdec and dec and strexp

    type longids = {funids: funid list, sigids: sigid list,
		    longstrids: longstrid list, longtycons: longtycon list,
		    longvids: longid list}

    val fid_topdec : topdec -> longids
    val fid_strexp : strexp -> longids
    val fid_dec    : dec    -> longids
				

    type StringTree
    val layout_longids : longids -> StringTree

  end