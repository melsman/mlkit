infix 1 seq

fun e1 seq e2 = e2;
fun tst0 s s' = (s ^ "    \t" ^ s' ^ "\n")
fun tstOk s f = tst0 s ((f () seq "OK") handle Fail s => "WRONG - " ^ s | _ => "WRONG - ")
fun tstBool s f = tst0 s ((if f () then "OK" else "WRONG") handle Fail s => "WRONG - " ^ s | _ => "WRONG - ")

fun ppTestRes [] = ""
  | ppTestRes (x::xs) = x ^ "<br>\n" ^ (ppTestRes xs)

(*** Creating table with clob field and simple db_test package ***)
val dmlCreateTable =
  [tstOk "create test sequence" (fn () =>Db.dml `create sequence db_clob_test_seq`),
   tstOk "create test table" (fn () =>Db.dml `create table db_clob_test ( id int primary key, 
                                                                          text clob, 
                                                                          text2 varchar2(4000) )`),
   tstOk "create package db_test" (fn () => Db.exec `
     create or replace package db_test 
     as
       procedure add (
         text in db_clob_test.text2%TYPE
       );
     end db_test;
     `),
   tstOk "create package body db_test" (fn () => Db.exec `
     create or replace package body db_test 
     as
       procedure add (
         text in db_clob_test.text2%TYPE
       )
       is
       begin
         insert into db_clob_test (id, text2) values (db_clob_test_seq.nextval, add.text);
       end add;
     end db_test;
     `)]

(*** Testing procedure calls ***)
val dmlProd = [tstOk "procedure add1" (fn () => Db.execSp [`db_test.add('Ny tekst')`]),
	       tstBool "procedure add2" (fn () => Db.oneField `select count(id) 
                                                                 from db_clob_test 
                                                                where text2 = 'Ny tekst'` = "1"),
	       tstOk "procedure add3" (fn () => Db.execSp [`db_test.add('Ny tekst2')`,
							   `db_test.add('Ny tekst2')`,
							   `db_test.add('Ny tekst2')`]),
	       tstBool "procedure add4" (fn () => Db.oneField `select count(id) 
                                                                 from db_clob_test 
                                                                where text2 = 'Ny tekst2'` = "3"),
	       tstOk "procedure add5" (fn () => Db.execSp [])]

(*** Testing clobs ***)
val dmlInsertRows =
  let
    fun tryString(s,acc) =
      tryString (s^s,(Db.dml `insert into db_clob_test (id, text) values (db_clob_test_seq.nextval, ^(Db.qqq s))` 
		      seq "Inserted string of size " ^ (Int.toString (String.size s))) :: acc)
      handle Fail err => List.rev("Failed on insert string of size " ^ 
				  (Int.toString (String.size s)) ^ "[" ^ err ^ "]" :: acc)
	| _ => List.rev("Failed on insert string of size " ^ (Int.toString (String.size s)) :: acc)
  in
    (*tryString("ABCDEF",[])*) []
  end

(*** End of test ***)
val dmlDropTable =
  [tstOk "drop package db_test" (fn () => Db.dml `drop package db_test`),
   tstOk "drop test sequence" (fn () => Db.dml `drop sequence db_clob_test_seq`),
   tstOk "drop test table" (fn () => Db.dml `drop table db_clob_test`)]

val _ = Page.return "Testing Oracle Clobs" `

The script sends a series of SQL statements to the database;
the result is shown below.<p>

<b>Notice:</b> This test is developed specifically for the Oracle
database and tests how large objects we can insert into clob-fields.

<h2>Creating Test Table and Package</h2>
^(ppTestRes dmlCreateTable)

<h2>Using procedure</h2>
^(ppTestRes dmlProd)

<h2>Inserting Rows</h2>
^(ppTestRes dmlInsertRows)

<h2>Dropping Test Table</h2>
^(ppTestRes dmlDropTable)<br>
`