-- $Id$

/* ======================================================================
   package scs_test

   History:
   141102 Kennie Nybo Pontoppidan <kennie@it-c.dk> Added comments
   171002 Niels Hallenberg <nh@it.edu> Created package
====================================================================== */
create or replace package scs_test
as
  /* ---------------
     procedure print
     ---------------
     prints a string on the screen (no new line)
  */
  procedure print (
    s varchar2
  );

  /* ----------------
     procedure printl
     ----------------
     prints a string on the screen (with new line)
  */
  procedure printl (
    s varchar2
  );

  /* ------------------
     procedure testBool
     ------------------
     Given a name 'testname', number 'testcase' and a boolean 'exp'
     If 'exp' is true then
       'ok: 'testname' testcase 'number'
     is printed. 
     If 'exp' is false then
       'error: 'testname' testcase 'number'
     is printed on the screen
  */
  procedure testBool(
    testname varchar2,
    testcase integer,
    exp boolean
  );

  /* -----------------
     procedure testExn
     -----------------
     This procedure is used to test whether exceptions are thrown as expected.
     Given a name 'testname', number 'testcase', a string 'exp' 
     and a flag all_exns, it executes the content of the string
       example: EXP => '
		  begin
		    ucs_project.addContact( rel_id => 123, project_id => 234 );
		  end; '
     If the block throws an ScsDbExn exception then 
       ok: 'testname' testcase 'number' [-20000]
     is printed
     If the block terminates without throwing any exceptions then 
       error: 'testname' testcase 'number'
     The flag all_exns can have values 't' or 'f'
     If all_exns is 't' and another exception than ScsDbExn is thrown then
        ok: 'testname' testcase 'number' [exeption_number]
     is printed. Else
        error: 'testname' testcase 'number' [exeption_number]
     is printed.
  */
  procedure testExn (
    testname varchar2,
    testcase integer,
    exp varchar2,
    all_exns varchar2
  );

  /* ------------------
     procedure testUnit
     ------------------
     This procedure is used to test whether something does not throw any 
     exceptions. Given a name 'testname', number 'testcase' and a string 'exp',
     it executes the content of the string (as in testExn).
     If no exception were thrown then
       'ok: 'testname' testcase 'number'
     is printed. Else
        error: 'testname' testcase 'number' [exeption_number]
     is printed.
  */
  procedure testUnit (
    testname varchar2,
    testcase integer,
    exp varchar2
  );

end scs_test;
/
show errors



/* ======================================================================
   package bodies begin here
====================================================================== */
create or replace package body scs_test
as
  procedure print (
    s varchar2
  )
  is
  begin
    dbms_output.put( s );
  end print;

  procedure printl (
    s varchar2
  )
  is
  begin
    dbms_output.put_line( s );
  end printl;

  procedure testBool(
    testname varchar2,
    testcase integer,
    exp boolean
  ) 
  is
    str varchar(7);
  begin
    if exp = TRUE then
      str := 'ok: ';
    else
      str := 'error: ';
    end if;
    printl( str || testname || ' testcase ' || to_char(testcase) );    
  end testBool;

  procedure testExn(
    testname varchar2,
    testcase integer,
    exp varchar2,
    all_exns varchar2 default 'f'
  ) 
  is
    ScsDbExn exception;
    -- The number -20000 must be similar to scs.ScsDbExn defined in
    -- scs-create.sql. You can't refer to constants in the pragma
    -- directive!
    pragma exception_init (ScsDbExn, -20000 );
  begin
    execute immediate testExn.exp;
    printl( 'error: ' || testname || ' testcase ' || to_char(testcase) );    
  exception
    when ScsDbExn then
      printl( 'ok: ' || testname || ' testcase ' || to_char(testcase) || '[' || SQLCODE || ']');
    when others then
      if all_exns = 'f' then
        printl( 'error: ' || testname || ' testcase ' || to_char(testcase) || '[' || SQLERRM || ']');
      else
        printl( 'ok: ' || testname || ' testcase ' || to_char(testcase) || '[' || SQLCODE || ']');
      end if;
  end testExn;

  procedure testUnit(
    testname varchar2,
    testcase integer,
    exp varchar2
  ) 
  is
  begin
    execute immediate testUnit.exp;
    printl( 'ok: ' || testname || ' testcase ' || to_char(testcase) );
  exception
    when others then
      printl( 'error: ' || testname || ' testcase ' || to_char(testcase) || '[' || SQLCODE || ']');
  end testUnit;

end scs_test;
/ 
show errors









