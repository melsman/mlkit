signature DB_CLOB =
  sig
    val insert    : quot -> string
    val insert_fn : quot -> (Db.db -> string)
(*    val update : string -> quot -> Ns.status*)
    val select    : string -> quot
    val select_fn : string -> (Db.db -> quot)
(*    val delete : string -> Ns.status*)
  end

structure DbClob :> DB_CLOB =
  struct
    fun insert_fn q = 
      let
        fun split s = if Substring.size s <= 4000
			then [Substring.string s]
		      else 
			let 
			  val (s1,s2) = Substring.splitAt(s,4000)
			in
			  Substring.string s1 :: split s2
			end
val _ = Ns.log (Ns.Notice, "before split")
	val strs = split (Substring.all (Quot.toString q))
val _ = Ns.log (Ns.Notice, "before split")
      in
	fn db =>
	  let
	    val clob_id = Int.toString (Db.seqNextval "db_clob_id_seq")
	  in
	    List.foldl (fn (s,idx) => 
			(Db.dmlDb(db,`insert into db_clob (clob_id,idx,text) 
				      values (^(Db.valueList[clob_id,Int.toString idx,s]))`);
			 idx+1)) 0 strs;
	    clob_id
	  end
      end
    val insert = Db.dmlTrans o insert_fn

    fun select_fn clob_id =
      fn db =>
      Db.foldDb (db,fn (g,acc) => acc ^^ `^(g "text")`,``,`select text from db_clob  
                                                            where clob_id=^(Db.qq' clob_id)
                                                            order by idx`)

    fun select clob_id = Db.wrapDb (select_fn clob_id)
  end