signature DB_CLOB =
  sig
    val insert    : quot -> string                    (* return fresh clob_id and inserts in db_clob table *)
    val insert_fn : quot -> (Db.db -> string)         (* lamba version of insert to use as part of a larger transaction *)
    val update    : string -> quot -> unit            (* update the clob identified as clob_id *)
    val update_fn : string -> quot -> (Db.db -> unit) (* lambda version of update to use as part of a larger transaction *)
    val select    : string -> quot                    (* select a Clob given a clob_id *)
    val select_fn : string -> (Db.db -> quot)         (* lamda version of select - to use in a larger transaction *)
    val delete    : string -> unit                    (* delete clob given a clob_id *)
    val delete_fn : string -> (Db.db -> unit)         (* lambda version of delete - to use in a larger transaction *)
  end

structure DbClob :> DB_CLOB =
  struct
    fun insert_fn' (clob_id:string) (q:quot) : Db.db -> string = 
      let
        fun split s = if Substring.size s <= 4000
			then [Substring.string s]
		      else 
			let 
			  val (s1,s2) = Substring.splitAt(s,4000)
			in
			  Substring.string s1 :: split s2
			end
	val strs = split (Substring.all (Quot.toString q))
      in
	fn db =>
	    (List.foldl (fn (s,idx) => 
			 (Db.dmlDb db `insert into db_clob (clob_id,idx,text) 
                                         values (^(Db.valueList[clob_id,Int.toString idx,s]))`;
			  idx+1)) 0 strs;
	     clob_id)
      end
    fun insert_fn q = insert_fn' (Int.toString (Db.seqNextval "db_clob_id_seq")) q
    val insert = Db.dmlTrans o insert_fn

    fun delete_fn (clob_id : string) : Db.db -> unit =
      fn db => Db.dmlDb db `delete from clob where clob_id = ^(Db.qqq clob_id)`
    fun delete clob_id = Db.dmlTrans (delete_fn clob_id)

    fun update_fn clob_id q = fn db => (delete_fn clob_id db; insert_fn' clob_id q db; ())
    fun update clob_id q = Db.dmlTrans (update_fn clob_id q)

    fun select_fn clob_id =
      fn db =>
      Db.foldDb db (fn (g,acc) => acc ^^ `^(g "text")`) `` `select text from db_clob  
                                                            where clob_id=^(Db.qqq clob_id)
                                                            order by idx`
    fun select clob_id = Db.wrapDb (select_fn clob_id)
  end