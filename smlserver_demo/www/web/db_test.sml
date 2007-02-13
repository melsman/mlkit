val _ = Web.return
`<html>
  <head><title>Testing WEB_DB</title></head>
  <body><h2>Testing the Database Interface (signature WEB_DB)</h2>

The script sends a series of SQL statements to the database;
the result is shown below.<p>

<b>Notice:</b> If you are using MySQL, errors in
the sections testing <i>sequences</i>, <i>panicDmlTrans</i>, and
<i>dmlTrans</i> are expected due to the lack of sequences 
and transactions in MySQL.<p>`

infix 1 seq

local
  val errs = ref 0
  fun add_err () = (errs := !errs + 1; "WRONG")
  fun add_err' s = (errs := !errs + 1; "WRONG - " ^ s)
in
  fun pp_errs() = 
    if !errs = 0 then 
      "<b>There were no errors.</b>" 
    else
      "<b>There were " ^ (Int.toString (!errs)) ^ " error(s).</b>"
  fun e1 seq e2 = e2;
  fun tst0 s s' = let val str = s ^ " &mdash; " ^ s' 
		  in Web.log(Web.Notice,str); Web.Conn.write(str ^ "<br>\n")
		  end
  fun tstOk s f = tst0 s ((f () seq "OK") handle Fail s => add_err' s | _ => add_err())
  fun tstBool s f = tst0 s ((if f () then "OK" else add_err' "false") handle Fail s => add_err' s | _ => add_err())
  fun tstFail s f = tst0 s ((f () seq add_err()) handle Fail s => "OK - " ^ s | _ => add_err())
end

fun log x = Web.log(Web.Debug, x)

val _ = Web.write `<h2>The function <code>dml</code></h2>`
val dmlTest =
  [tstOk "dmlA1" (fn () =>Db.dml `create table db_test ( id int primary key )`),
   (* Creating the same table again should fail *)
   tstFail "dmlA2" (fn () =>Db.dml `create table db_test ( id int primary key )`),
   (* Syntax error *)
   tstFail "dmlA3" (fn () => Db.dml `createe table db_test ( id int primary key )`),
   (* Inserting Rows *)
   tstOk "dmlB1" (fn () => Db.dml `insert into db_test (id) values (1)`),
   (* Fail on primary key constraint *)
   tstFail "dmlB2" (fn () => Db.dml `insert into db_test (id) values (1)`),
   (* Updating Rows *)
   tstOk "dmlC1" (fn () => Db.dml `update db_test set id = 42 where id = '1'`),
   (* No fail when no rows are updated *)
   tstOk "dmlC2" (fn () => Db.dml `update db_test set id = 3 where id = '1'`),
   tstOk "dmlE1" (fn () => Db.dml `drop table db_test`),
   (* Dropping the same table again should fail *)
   tstFail "dmlE2" (fn () => Db.dml `drop table db_test`)]

val _ = Web.write `<h2>The function <code>maybeDml</code></h2>`
val maybeDmlTest =
  [tstBool "maybeDmlA1" (fn () =>Db.maybeDml `create table db_test ( id int primary key )` = ()),
   (* Creating the same table again should fail but () is returned*)
   tstBool "maybeDmlA2" (fn () =>Db.maybeDml `create table db_test ( id int primary key )` = ()),
   (* Syntax error - error is suppressed *)
   tstBool "maybeDmlA3" (fn () => Db.maybeDml `createe table db_test ( id int primary key )` = ()),
   (* Inserting Rows *)
   tstBool "maybeDmlB1" (fn () => Db.maybeDml `insert into db_test (id) values (1)` = ()),
   (* Fail on primary key constraint - error is suppressed *)
   tstBool "maybeDmlB2" (fn () => Db.maybeDml `insert into db_test (id) values (1)` = ()),
   (* Updating Rows *)
   tstBool "maybeDmlC1" (fn () => Db.maybeDml `update db_test set id = 42 where id = '1'` = ()),
   (* No rows are updated *)
   tstBool "maybeDmlC2" (fn () => Db.maybeDml `update db_test set id = 3 where id = '1'` = ()),
   (* Drop the table *)
   tstBool "maybeDmlE1" (fn () => Db.maybeDml `drop table db_test` = ()),
   (* Dropping the same table again should fail, but error is suppressed *)
   tstBool "maybeDmlE2" (fn () => Db.maybeDml `drop table db_test` = ())]

val _ = Web.write `<h2>The function <code>panicDml</code></h2>`
local
  val f_count = ref 0
  fun f_panic _ = f_count := !f_count + 1
  val panicDml = Db.panicDml f_panic
in
  val panicDmlTest =
    [tstBool "panicDmlA1" (fn () => panicDml `create table db_test ( id int primary key )` = () andalso !f_count = 0),
     (* Creating the same table again should fail but () is returned*)
     tstBool "panicDmlA2" (fn () =>panicDml `create table db_test ( id int primary key )` = () andalso !f_count = 1),
     (* Syntax error - error is suppressed *)
     tstBool "panicDmlA3" (fn () => panicDml `createe table db_test ( id int primary key )` = () andalso !f_count = 2),
     (* Inserting Rows *)
     tstBool "panicDmlB1" (fn () => panicDml `insert into db_test (id) values (1)` = () andalso !f_count = 2),
     (* Fail on primary key constraint - error is suppressed *)
     tstBool "panicDmlB2" (fn () => panicDml `insert into db_test (id) values (1)` = () andalso !f_count = 3),
     (* Updating Rows *)
     tstBool "panicDmlC1" (fn () => panicDml `update db_test set id = 42 where id = '1'` = () andalso !f_count = 3),
     (* No rows are updated *)
     tstBool "panicDmlC2" (fn () => panicDml `update db_test set id = 3 where id = '1'` = () andalso !f_count = 3),
     (* Drop the table *)
     tstBool "panicDmlE1" (fn () => panicDml `drop table db_test` = () andalso !f_count = 3),
     (* Dropping the same table again should fail, but error is suppressed *)
     tstBool "panicDmlE2" (fn () => panicDml `drop table db_test` = () andalso !f_count = 4)]
end

val _ = Web.write `<h2>The function <code>dmlTrans</code></h2>`
val dmlTransTest =
  let
    fun db_testL () = let val a = Db.list (fn g => g "id") `select id from db_test order by id` 
                      in (List.app (fn x => (log x;())) a; a)
                      end
  in
    [tstOk "dmlTransA1" (fn () => Db.dml `create table db_test ( id int primary key )`),
     (* Unique Constraint Violated on key id *)
     tstFail "dmlTransA2" (fn () => Db.Handle.dmlTrans (fn db => 
						 (Db.Handle.dmlDb db `insert into db_test (id) values ('3')`;
						  Db.Handle.dmlDb db `insert into db_test (id) values ('4')`;
						  Db.Handle.dmlDb db `insert into db_test (id) values ('4')`))),
     tstBool "dmlTransA3" (fn () => db_testL() = []),
     (* Ok transaction *)
     tstOk "dmlTransA4" (fn () => Db.Handle.dmlTrans (fn db => 
					       (Db.Handle.dmlDb db `insert into db_test (id) values ('3')`;
						Db.Handle.dmlDb db `insert into db_test (id) values ('4')`;
						Db.Handle.dmlDb db `insert into db_test (id) values ('5')`))),
     tstBool "dmlTransA5" (fn () => db_testL() = ["3","4","5"]),
     (* Syntax Error - and the previous content is maintained *)
     tstFail "dmlTransA6" (fn () => Db.Handle.dmlTrans (fn db => 
						 (Db.Handle.dmlDb db `delete from db_test`;
						  Db.Handle.dmlDb db `inserte into db_test (id) values ('4')`;
						  Db.Handle.dmlDb db `insert into db_test (id) values ('4')`))),
     tstBool "dmlTransA7" (fn () => db_testL() = ["3","4","5"])]
  end

val _ = Web.write `<h2>The function <code>panicDmlTrans</code></h2>`
val panicDmlTransTest =
  let
    fun db_testL () = Db.list (fn g => g "id") `select id from db_test order by id`
    val f_count = ref 0
    fun f_panic _ = (f_count := !f_count + 1; true)
    val panicDml = Db.Handle.panicDmlTrans f_panic
  in
    [tstOk "panicDmlTransA1" (fn () => Db.dml `delete from db_test`),
     (* Unique Constraint Violated on key id *)
     tstBool "panicDmlTransA2" (fn () => panicDml (fn db => 
						   (Db.Handle.dmlDb db `insert into db_test (id) values ('3')`;
						    Db.Handle.dmlDb db `insert into db_test (id) values ('4')`;
						    Db.Handle.dmlDb db `insert into db_test (id) values ('4')`;
						    false)) andalso !f_count = 1),
     tstBool "panicDmlTransA3" (fn () => db_testL() = []),
     (* Ok transaction *)
     tstBool "panicDmlTransA4" (fn () => panicDml (fn db => 
						   (Db.Handle.dmlDb db `insert into db_test (id) values ('3')`;
						    Db.Handle.dmlDb db `insert into db_test (id) values ('4')`;
						    Db.Handle.dmlDb db `insert into db_test (id) values ('5')`;
						    true)) andalso !f_count = 1),
     tstBool "panicDmlTransA5" (fn () => db_testL() = ["3","4","5"]),
     (* Syntax Error - and the previous content is maintained *)
     tstBool "panicDmlTransA6" (fn () => panicDml (fn db => 
						   (Db.Handle.dmlDb db `delete from db_test`;
						    Db.Handle.dmlDb db `inserte into db_test (id) values ('4')`;
						    Db.Handle.dmlDb db `insert into db_test (id) values ('4')`;
						    false)) andalso !f_count = 2),
     tstBool "panicDmlTransA7" (fn () => db_testL() = ["3","4","5"])]
  end

val _ = Web.write `<h2>The function <code>fold</code></h2>`
val foldTest =
  [tstOk "delete" (fn () => Db.dml `delete from db_test`),
   tstOk "insert" (fn () => Db.dml `insert into db_test values ('3')`),
   tstOk "insert" (fn () => Db.dml `insert into db_test values ('4')`),
   tstOk "insert" (fn () => Db.dml `insert into db_test values ('5')`),
   tstBool "foldA1" (fn () => Quot.==(Db.fold (fn (g,acc) => acc ^^ ` ^(g "id")`) `` `select id from db_test order by id`,
				      ` 3 4 5`)),
   (* Syntax Error *)
   tstFail "foldA2" (fn () => Quot.==(Db.fold (fn (g,acc) => acc ^^ ` ^(g "id")`) `` 
					       `selecte id from db_test order by id`,
				      ` 3 4 5`)),
   (* Empty Result *)
   tstBool "foldA3" (fn () => Quot.==(Db.fold (fn (g,acc) => acc ^^ ` ^(g "id")`) ``
					     `select id from db_test where id > 40 order by id`, ``))]

val _ = Web.write `<h2>The function <code>list</code></h2>`
val listTest = 
  [tstBool "listA1" (fn () => Db.list (fn g => g "id") `select id from db_test order by id` = ["3","4","5"]),
   (* Syntax Error *)
   tstFail "listA2" (fn () => Db.list (fn g => g "id") `selecte id from db_test order by id` = ["3","4","5"]),
   (* Empty Result *)
   tstBool "listA3" (fn () => Db.list (fn g => g "id") `select id from db_test where id > 40 order by id` = [])]

val _ = Web.write `<h2>The function <code>app</code></h2>`
val appTest = 
let
  val f_count = ref 0
  fun f g = f_count := !f_count + Option.valOf(Int.fromString (g "id"))
in
  [tstBool "appA1" (fn () => (Db.app f `select id from db_test order by id`;
			      !f_count = 12)),
   (* Syntax Error *)
   tstFail "appA2" (fn () => Db.app f `selecte id from db_test order by id`),
   (* Empty Result *)
   tstBool "appA3" (fn () => (Db.list f `select id from db_test where id > 40 order by id`;
			      !f_count = 12))]
end

val _ = Web.write `<h2>The function <code>oneField</code></h2>`
val oneFieldTest =
  [tstBool "oneFieldA1" (fn () => Db.oneField (`select id from db_test where id = '3'`) = "3"),
   (* Fail on zero rows *)
   tstFail "oneFieldA2" (fn () => Db.oneField (`select id from db_test where id = '78'`)),
   (* Fail on two rows *)
   tstFail "oneFieldA3" (fn () => Db.oneField (`select id from db_test where id > '3'`)),
   (* Fail on zero fields - syntax error*)
   tstFail "oneFieldA4" (fn () => Db.oneField (`select from db_test where id > '3'`)),
   (* Fail on more that one field*)
   tstFail "oneFieldA5" (fn () => Db.oneField (`select id, id+id as idd from db_test where id > '3'`))]

val _ = Web.write `<h2>The function <code>zeroOrOneField</code></h2>`
val zeroOrOneFieldTest =
  [(* One row, one field *)
   tstBool "zeroOrOneFieldA1" (fn () => Db.zeroOrOneField (`select id from db_test where id = '3'`) = SOME "3"),
   (* Zero rows, one field *)
   tstBool "zeroOrOneFieldA2" (fn () => Db.zeroOrOneField (`select id from db_test where id > '33'`) = NONE),
   (* Zero rows, many fields *)
   tstBool "zeroOrOneFieldA3" (fn () => Db.zeroOrOneField (`select id, id+id as idd from db_test where id > '33'`) = NONE),
   (* Fail on two rows *)
   tstFail "zeroOrOneFieldA4" (fn () => Db.zeroOrOneField (`select id from db_test where id > '3'`)),
   (* Fail on zero fields - syntax error *)
   tstFail "zeroOrOneFieldA5" (fn () => Db.zeroOrOneField (`select from db_test where id > '3'`)),
   (* Fail on one row and more that one field *)
   tstFail "zeroOrOneFieldA6" (fn () => Db.zeroOrOneField (`select id, id+id as idd from db_test where id = '3'`))]

val _ = Web.write `<h2>The function <code>oneRow</code></h2>`
val oneRowTest =
  [(* One row, one field *)
   tstBool "oneRowA1" (fn () => Db.oneRow (`select id from db_test where id = '3'`) = ["3"]),
   (* Zero rows *)
   tstFail "oneRowA2" (fn () => Db.oneRow (`select id from db_test where id > '33'`)),
   (* Fail on two rows *)
   tstFail "oneRowA3" (fn () => Db.oneRow (`select id from db_test where id > '3'`)),
   (* Fail on zero fields - syntax error *)
   tstFail "oneRowA4" (fn () => Db.oneRow (`select from db_test where id > '3'`)),
   (* One row, two fields *)
   tstBool "oneRowA5" (fn () => Db.oneRow (`select id, id+id as idd from db_test where id = '3'`) = ["3","6"])]

val _ = Web.write `<h2>The function <code>oneRow'</code></h2>`
val oneRow'Test =
  [(* One row, one field *)
   tstBool "oneRow'A1" (fn () => Db.oneRow' (fn g => g "id") `select id from db_test where id = '3'` = "3"),
   (* Zero rows *)
   tstFail "oneRow'A2" (fn () => Db.oneRow' (fn g => g "id") `select id from db_test where id > '33'`),
   (* Fail on two rows *)
   tstFail "oneRow'A3" (fn () => Db.oneRow' (fn g => g "id") `select id from db_test where id > '3'`),
   (* Fail on zero fields - syntax error *)
   tstFail "oneRow'A4" (fn () => Db.oneRow' (fn g => g "id") `select from db_test where id > '3'`),
   (* One row, two fields *)
   tstBool "oneRow'A5" (fn () => Db.oneRow' (fn g => (g "id", g "idd")) 
			`select id, id+id as idd from db_test where id = '3'` = ("3","6"))]

val _ = Web.write `<h2>The function <code>zeroOrOneRow</code></h2>`
val zeroOrOneRowTest =
  [(* One row *)
   tstBool "zeroOrOneRowA1" (fn () => Db.zeroOrOneRow (`select id from db_test where id = '3'`) = SOME ["3"]),
   (* Zero rows *)
   tstBool "zeroOrOneRowA2" (fn () => Db.zeroOrOneRow (`select id from db_test where id > '33'`) = NONE),
   (* Fail on two rows *)
   tstFail "zeroOrOneRowA3" (fn () => Db.zeroOrOneRow (`select id from db_test where id > '3'`)),
   (* Fail on zero fields - syntax error *)
   tstFail "zeroOrOneRowA4" (fn () => Db.zeroOrOneRow (`select from db_test where id > '3'`)),
   (* One row, two fields *)
   tstBool "zeroOrOneRowA5" (fn () => Db.zeroOrOneRow (`select id, id+id as idd from db_test where id = '3'`) = SOME ["3","6"])]

val _ = Web.write `<h2>The function <code>zeroOrOneRow'</code></h2>`
val zeroOrOneRow'Test =
  [(* One row *)
   tstBool "zeroOrOneRow'A1" (fn () => Db.zeroOrOneRow' (fn g => g "id") `select id from db_test where id = '3'` = SOME "3"),
   (* Zero rows *)
   tstBool "zeroOrOneRow'A2" (fn () => Db.zeroOrOneRow' (fn g => g "id") `select id from db_test where id > '33'` = NONE),
   (* Fail on two rows *)
   tstFail "zeroOrOneRow'A3" (fn () => Db.zeroOrOneRow' (fn g => g "id") `select id from db_test where id > '3'`),
   (* Fail on zero fields - syntax error *)
   tstFail "zeroOrOneRow'A4" (fn () => Db.zeroOrOneRow' (fn g => g "id") `select from db_test where id > '3'`),
   (* One row, two fields *)
   tstBool "zeroOrOneRow'A5" (fn () => Db.zeroOrOneRow' (fn g => (g "id",g "idd")) `select id, id+id as idd from db_test where id = '3'` 
			      = SOME ("3","6"))]

val _ = Web.write `<h2>The function <code>existsOneRow</code></h2>`
val existsOneRowTest =
  [(* Zero rows *)
   tstBool "existsOneRowA1" (fn () => Db.existsOneRow `select id from db_test where id > '40'` = false),
   (* One row *)
   tstBool "existsOneRowA2" (fn () => Db.existsOneRow `select id from db_test where id = '4'` = true),
   (* More than one row*)
   tstBool "existsOneRowA3" (fn () => Db.existsOneRow `select id from db_test where id > '3'` = true),
   (* Fail on zero fields, syntax error *)
   tstFail "existsOneRowA4" (fn () => Db.existsOneRow `select from db_test where id > '3'`)]

val _ = Web.write `<h2>Sequences</h2>`
val seqTest =
  let
    fun db_testL () = Db.list (fn g => g "id") `select id from db_test order by id`
  in
    [tstOk "create sequence" (fn () => Db.dml `create sequence t`),
     tstOk "drop table" (fn () => Db.dml `drop table db_test`),
     tstOk "create table" (fn () =>Db.dml `create table db_test ( id int primary key )`),
     tstOk "seqNextvalExp" (fn () => Db.dml `insert into db_test values (^(Db.seqNextvalExp "t"))`),
     tstBool "seqNextvalExp" (fn () => db_testL() = ["1"]),
     tstFail "seqCurrvalExp" (fn () => Db.dml `insert into db_test values (^(Db.seqCurrvalExp "t"))`),
     tstBool "seqCurrvalExp" (fn () => db_testL() = ["1"]),
     tstBool "seqNextval" (fn () => Db.seqNextval "t" = 2),
     tstBool "seqCurrval" (fn () => Db.seqCurrval "t" = 2),
     tstOk "drop sequence" (fn () => Db.dml `drop sequence t`),
     tstOk "drop table" (fn () => Db.dml `drop table db_test`)]
  end

val _ = Web.write `<h2>Various Functions</h2>`
val miscTest =
  let
    val d = Date.fromTimeLocal(Time.now())
  in
    [tstOk "create table" (fn () => Db.dml `create table db_test (d ^(Db.timestampType))`),
     tstOk "sysdateExp" (fn () => Db.dml `insert into db_test values (^(Db.sysdateExp))`),
     tstBool "qq" (fn () => Db.qq "hi" = "hi"),
     tstBool "qq" (fn () => Db.qq "'h'i'" = "''h''i''"),
     tstBool "qqq" (fn () => Db.qqq "hi" = "'hi'"),
     tstBool "qqq" (fn () => Db.qqq "'h'i'" = "'''h''i'''"),
     tstOk "fromDate" (fn () => Db.dml `delete from db_test`),
     tstOk "fromDate" (fn () => Db.dml `insert into db_test values (^(Db.fromDate d))`),
     tstBool "toDate" (fn () => 
		       case Db.toDate(Db.oneField `select ^(Db.toDateExp "d") from db_test`) of 
			 SOME d_db => Date.year d_db = Date.year d andalso 
			   Date.month d_db = Date.month d andalso 
			   Date.day d_db = Date.day d 
		       | NONE => false),
     tstBool "toTimestamp" (fn () => 
			    case Db.toTimestamp(Db.oneField `select ^(Db.toTimestampExp "d") from db_test`) of 
			      SOME t_db => Date.compare(t_db,d) = EQUAL 
			    | NONE => false),
     tstBool "toDate" (fn () => case Db.toDate "Not a date" of SOME _ => false | NONE => true),
     tstOk "drop table" (fn () => Db.dml `drop table db_test`),
     tstOk "create table" (fn () => Db.dml `create table db_test (t varchar(100))`),
     tstOk "valueList" (fn () => Db.dml `insert into db_test values (^(Db.valueList ["hi"]))`),
     tstBool "valueList" (fn () => Db.oneField `select t from db_test` = "hi"),
     tstOk "valueList" (fn () => Db.dml `delete from db_test`),
     tstOk "valueList" (fn () => Db.dml `insert into db_test values (^(Db.valueList ["'h'i'"]))`),
     tstBool "valueList" (fn () => Db.oneField `select t from db_test` = "'h'i'"),
     tstOk "setList" (fn () => Db.dml `update db_test set ^(Db.setList [("t","hi")])`),
     tstBool "setList" (fn () => Db.oneField `select t from db_test` = "hi"),
     tstOk "setList" (fn () => Db.dml `update db_test set ^(Db.setList [("t","'h'i'")])`),
     tstBool "setList" (fn () => Db.oneField `select t from db_test` = "'h'i'")]
  end

val _ = Web.write `<h2>Table Dropping</h2>`

val dmlTransE1 = tstOk "dmlTransE1" (fn () => Db.dml `drop table db_test`)

val _ = Web.write `<h2>Summary</h2>^(pp_errs())<p>`

val _ = Web.write 
`<hr><i>Served by <a href=http://www.smlserver.org>SMLserver</a></i>,
<i><a href="/web/index.sml">Back to index page</a>.</i>
</body></html>`
