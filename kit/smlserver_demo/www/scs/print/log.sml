val dicts = Db.list (fn g => (g "login", g "print_id", g "category", g "doc_type", g "note", 
			      g "time_stamp", g "deleted_p"))
		     `select login, print_id, category, doc_type, note, time_stamp, deleted_p
                        from scs_print_log,auth_user 
                       where scs_print_log.user_id = auth_user.user_id
                       order by deleted_p, time_stamp desc`

val _ = ScsPage.returnPg "Print Log" 
  (case dicts of
     [] => `There are no printed documents`
   | _ => (ScsWidget.lineTable
	   {hdcolor="silver",row_col1="silver",row_col2="lightgrey",
	    header=`<th>Print Job</th><th>User</th><th>Category</th><th>Document Type</th>
	    <th>Note</th><th>Time Stamp</th><th>Deleted</th><th>&nbsp;</th>`,
	    align="center",footer=``} 
	   (fn (login,print_id,category,doc_type,note,time_stamp,deleted_p) => 
	    `<td align=right>^print_id</td>
	    <td>^login</td>
	    <td>^category</td>
	    <td>^doc_type</td>
	    <td>^note</td>
	    <td>^(ScsDb.ppDate time_stamp)</td>
	    <td>^deleted_p(<a href="toggle_deleted.sml?print_id=^(Ns.encodeUrl print_id)">toggle</a>)</td>
	    <td><a href="show_doc.sml?print_id=^(Ns.encodeUrl print_id)">show</a></td>`) dicts))

