signature SCS_SHOW_PROGRESS =
  sig
    (* The status that a computing at any time can have. *)
    datatype progress_status = 
      computing | done_show_content | done_redirect | timed_out

    val progress_status_from_DB : string -> progress_status
    val progress_status_to_DB   : progress_status -> string

    type content_record = 
      {sp_id                    : int,
       user_id                  : int,
       status                   : progress_status,
       start_time               : Date.date,
       time_out_sec             : int,
       content_head             : string,
       content_body             : string,
       redirect_url_on_success  : string,
       redirect_url_on_time_out : string,
       refresh_rate_sec         : int}

    val url_status_page : string

    (* [getContent sp_id] returns a content_record given a
        sp_id. Returns NONE if no record matches sp_id *)
    val getContent : int -> content_record option

    (* [create (progress_status,time_out_sec,content_head,content_body,
                url_on_success,
                url_on_time_out,refresh_rate_sec)] inserts a new
        record in DB and returns a sp_id for that record. *)
    val create : progress_status * int * quot * quot * string * string * int -> int

    (* [updateContent (sp_id,content_head,content_body)] updates 
        record sp_id with new
        content. Never fails, that is, ignore if no record matching
        sp_id exists. *)
    val updateContent : int * quot * quot -> unit

    (* [updateStatus (sp_id,status)] updates record matching sp_id
        with new status. Never fails, that is, does nothing if
        record does not exists. *)
    val updateStatus  : int * progress_status -> unit

    (* [returnPage sp_id] returns HTML page with content
        corresponding the record sp_id. Puts a refresh-rate in the
        HTML code. *)
    val returnPage : int -> unit
  end;

structure ScsShowProgress :> SCS_SHOW_PROGRESS =
  struct
    datatype progress_status = 
      computing | done_show_content | done_redirect | timed_out

    fun progress_status_from_DB "computing" = computing
      | progress_status_from_DB "done_show_content" = done_show_content
      | progress_status_from_DB "done_redirect" = done_redirect
      | progress_status_from_DB "timed_out" = timed_out
      | progress_status_from_DB s =
      ScsError.panic `ScsShowProgress.progress_status_from_DB: can't convert ^s`

    fun progress_status_to_DB computing = "computing"
      | progress_status_to_DB done_show_content = "done_show_content"
      | progress_status_to_DB done_redirect = "done_redirect"
      | progress_status_to_DB timed_out = "timed_out"

    type content_record = 
      {sp_id                    : int,
       user_id                  : int,
       status                   : progress_status,
       start_time               : Date.date,
       time_out_sec             : int,
       content_head             : string,
       content_body             : string,
       redirect_url_on_success  : string,
       redirect_url_on_time_out : string,
       refresh_rate_sec         : int}

    val url_status_page = "/scs/util/scs_sp_page.sml"

    fun getContent sp_id = 
      let
	val content_sql =
	  `select sp_id,
                  user_id,
		  status,
		  ^(Db.toTimestampExp "start_time") as start_time,
		  time_out_sec,
		  content_head,
		  content_body,
		  redirect_url_on_success,
		  redirect_url_on_time_out,
		  refresh_rate_sec
             from scs_sp_content
            where sp_id = '^(Int.toString sp_id)'`
	fun f g =
	  {sp_id                    = ScsData.gToInt g "sp_id",
	   user_id                  = ScsData.gToInt g "user_id",
	   status                   = progress_status_from_DB (g "status"),
	   start_time               = ScsData.gToTimestamp g "start_time",
	   time_out_sec             = ScsData.gToInt g "time_out_sec",
	   content_head             = g "content_head",
	   content_body             = g "content_body",
	   redirect_url_on_success  = g "redirect_url_on_success",
	   redirect_url_on_time_out = g "redirect_url_on_time_out",
	   refresh_rate_sec         = ScsData.gToInt g "refresh_rate_sec"}

      in
	SOME(Db.oneRow' f content_sql)
	handle _ => NONE
      end		  

    fun create (progress_status, time_out_sec, content_head, content_body,
		url_on_success, url_on_time_out, refresh_rate_sec) =
      let
	val sp_id = ScsDb.newObjId()
	val ins_dml =
	  `insert into scs_sp_content
	     (sp_id,user_id,status,start_time,time_out_sec,content_head,content_body,
	      redirect_url_on_success,redirect_url_on_time_out,refresh_rate_sec)
           values
	     ('^(Int.toString sp_id)',
	      '^(Int.toString (ScsLogin.user_id()))',
	      '^(progress_status_to_DB computing)',
	      ^(Db.fromDate (ScsDate.now_local())),
	      '^(Int.toString time_out_sec)',
	      ^(Db.qqq (Quot.toString content_head)),
	      ^(Db.qqq (Quot.toString content_body)),
	      ^(Db.qqq url_on_success),
	      ^(Db.qqq url_on_time_out),
	      '^(Int.toString refresh_rate_sec)')`
      in
	ScsError.wrapPanic Db.dml ins_dml;
	sp_id
      end

    fun updateContent (sp_id,content_head,content_body) =
      let
	val upd_dml =
	  `update scs_sp_content
	      set ^(Db.setList [("content_head",Quot.toString content_head),
				("content_body",Quot.toString content_body)])
            where sp_id = '^(Int.toString sp_id)'`
      in
	ScsError.wrapPanic Db.dml upd_dml
      end

    fun updateStatus (sp_id,status) =
      let
	val upd_dml =
	  `update scs_sp_content
	      set ^(Db.setList [("status",progress_status_to_DB status)])
	    where sp_id = '^(Int.toString sp_id)'`
      in
	ScsError.wrapPanic Db.dml upd_dml
      end

    fun returnPage sp_id =
      let
	val user_id = ScsLogin.user_id()
	fun returnPage' refresh_header (content_rec:content_record) =
	  (Ns.return (`<html>
		      <head>
		      ` ^^ refresh_header ^^ `
		      ^(#content_head content_rec)
		      </head>
		      <body>
		      ^(#content_body content_rec)
		      </body>
		      </html>`);
	   ())
      in
	case getContent sp_id of
	  NONE => ScsError.errorMsg (ScsDict.s' [(ScsLang.da,`Beklager, men der er opstået en fejl`),
						 (ScsLang.en,`Sorry, but an error happened`)])
	| SOME content_rec => 
	    case (#status content_rec) of
	      computing => 
		if Date.compare(ScsDate.now_local(),
				ScsDate.add_secs (#start_time content_rec) 
				(#time_out_sec content_rec)) = General.GREATER then
		  (updateStatus (sp_id,timed_out);
		   returnPage sp_id)
		else
		  returnPage'
		  `<META HTTP-EQUIV="Refresh" 
   		      CONTENT="^(Int.toString (#refresh_rate_sec content_rec));
                        URL=^(url_status_page ^ "?sp_id=" ^ (Int.toString (#sp_id content_rec)))">` 
		      content_rec
	  | done_show_content => returnPage' ` ` content_rec
	  | done_redirect => (Ns.returnRedirect (#redirect_url_on_success content_rec);())
	  | timed_out => (Ns.returnRedirect (#redirect_url_on_time_out content_rec);())
      end
  end

