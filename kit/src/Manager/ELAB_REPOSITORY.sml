(*$ELAB_REPOSITORY: TYNAME*)

signature ELAB_REPOSITORY =
  sig

    structure TyName : TYNAME

    type funid and InfixBasis and ElabBasis and realisation and name and longstrid
    type prjid = string

    val empty_infix_basis : InfixBasis

    val clear : unit -> unit
    val delete_entries : prjid * funid -> unit

	  (* Repository lookup's return the first entry for a (prjid,funid)
	   * which is reusable (i.e. where all export (ty-)names are
	   * marked generative.) In particular, this means that an
	   * entry which has been added, cannot be returned by a
	   * lookup, prior to executing `recover().' The integer
	   * provided by the lookup functions can be given to the
	   * overwrite functions for owerwriting a particular
	   * entry. *)

    val lookup_elab : (prjid * funid) -> 
      (int * (InfixBasis * ElabBasis * longstrid list * (realisation * TyName.Set.Set) * name list * 
	      InfixBasis * ElabBasis * realisation)) option

    val add_elab : (prjid * funid) * 
      (InfixBasis * ElabBasis * longstrid list * (realisation * TyName.Set.Set) * name list * 
       InfixBasis * ElabBasis * realisation) -> unit

    val owr_elab : (prjid * funid) * int * 
      (InfixBasis * ElabBasis * longstrid list * (realisation * TyName.Set.Set) * name list * 
       InfixBasis * ElabBasis * realisation) -> unit

    val recover : unit -> unit

          (* Before building a project the repository should be
	   * ``recovered'' meaning that all export names are marked
	   * generative (see NAME). Then when an entry is reused,
	   * export names are marked non-generative (for an entry to
	   * be reused all export names must be marked generative.) *)
  end