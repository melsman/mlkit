signature ULFILE =
    sig
	type bdec
	type location = string   (* request location (as seen in browser) *)
	type uofile = string     (* path to uofile on file system *)
	type smlfile = string

	datatype ul = SEQul of ul * ul
	  | SCRIPTSul of (uofile * location) list
	  | CODEFILESul of uofile list
	  | EMPTYul

	val pp_ul : ul -> string

	val from_bdec : (smlfile->uofile list) -> bdec -> ul
    end
