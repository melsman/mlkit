set serveroutput on; -- used by procedure printTest defined below
/
show errors

---------------------------------
-- scs_test package prototypes --
---------------------------------
create or replace package scs_test
as

  procedure printTest(
    testname varchar2,
    testcase integer,
    expression boolean
  ) ;

end scs_test;
/
show errors

---------------------------
-- scs_test package body --
---------------------------
create or replace package body scs_test
as

  procedure printTest(
    testname varchar2,
    testcase integer,
    expression boolean
  ) 
  is
    str varchar(7);
  begin
    if expression = TRUE then
      str := 'true: ';
    else
      str := 'false: ';
    end if;
    DBMS_output.put_line( str || testname || ' testcase ' || to_char(testcase) );    
  end printTest;

end scs_test;
/ 
show errors









