set serveroutput on

---------------------------------
-- scs_test package prototypes --
---------------------------------
create or replace package scs_test
as
  procedure print (
    s varchar2
  );

  procedure printl (
    s varchar2
  );

  procedure testBool(
    testname varchar2,
    testcase integer,
    exp boolean
  );

  procedure testExn (
    testname varchar2,
    testcase integer,
    exp varchar2,
    all_exns varchar2
  );
end scs_test;
/
show errors

---------------------------
-- scs_test package body --
---------------------------
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
        printl( 'error: ' || testname || ' testcase ' || to_char(testcase) || '[' || SQLCODE || SQLERRM || ']');
      else
        printl( 'ok: ' || testname || ' testcase ' || to_char(testcase) || '[' || SQLCODE || ']');
      end if;
  end testExn;

end scs_test;
/ 
show errors









