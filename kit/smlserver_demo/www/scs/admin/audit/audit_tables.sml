val tables = Db.list (fn g => List.nth (Option.valOf 
		      (RegExp.extract (RegExp.fromString "(.*)_AUDIT") (g "table_name")),0))
		      `select table_name 
                         from user_tables 
                        where lower(table_name) like '%_audit'`

val _ = ScsPage.returnPg "Audit Tables" (`

<h2>Audit Tables</h2>

This page will let you see all changes to one table of the
audited tables in the database over a specified period of time.<p>

 <b>It is
recommended that you start with a narrow time window and expand as
needed. Some tables are very large.</b><p> 
` ^^ (ScsWidget.formBox "../../audit/audit_table.sml" [("submit","Audit")] (`
<input type=hidden name="id" value="id">
<ul>
<li>What table do you want to audit:
` ^^ 
(ScsWidget.select (ListPair.zip(tables,tables)) "table_name") ^^ `

<p>

<li>When do you want to audit back to: (Leave blank to start at the begining of the table's history.)<br>
` ^^ (ScsWidget.intextDate NONE "start_date") ^^ `

<p>

<li>When do you want to audit up to:<br> ` ^^ 
(ScsWidget.intextDate NONE "end_date") ^^ `

<li>Limit rows to: ` ^^ 

(ScsWidget.intextVal 5 "50" "limit") ^^
` 

</ul>

<center>
<b>Note: if the table is very large, this may take a while.</b><br>
</center>
`)))

