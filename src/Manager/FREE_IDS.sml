(*$FREE_IDS*)

signature FREE_IDS =
  sig

    type ids and topdec and dec and strexp and id and tycon 
     and strid and funid and sigid

    val vids_of_ids:     ids -> id list
    val tycons_of_ids:   ids -> tycon list
    val strids_of_ids:   ids -> strid list
    val funids_of_ids:   ids -> funid list
    val sigids_of_ids:   ids -> sigid list

    val free_ids: topdec -> ids
    val free_ids_dec : dec -> ids
    val free_ids_strexp : strexp -> ids

    type StringTree
    val layout_ids : ids -> StringTree

  end