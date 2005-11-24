signature ULFILE =
    sig
	type location = string   (* request location (as seen in browser) *)
	type uofile = string     (* path to uofile on file system *)
	type smlfile = string
	type mlbfile = string

	datatype ul = SEQul of ul * ul
	  | SCRIPTSul of (uofile * location) list
	  | CODEFILESul of uofile list
	  | EMPTYul

	val pp_ul : ul -> string

	val from_mlbfile : (smlfile->uofile list) -> mlbfile -> ul
    end
