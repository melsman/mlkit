-- Some examples:

-- This should be turned into a decent test 2002-10-15, nh
declare
  i integer;
  s varchar2(40);
begin
  scs_test.printl('[Testing scs_random...]');
  for j in 1 .. 10
  loop
    select scs_random.rand_max(10) 
      into i
      from dual;
    scs_test.testBool('rand_max(10) = ' || i,j,i >= 1 and i <= 10);
  end loop;

  for j in 1 .. 10
  loop
    select scs_random.rand_string(10+j) 
      into s
      from dual;
    scs_test.testBool('rand_string(' || (10+j) || ') = ' || s,10+j,length(s) = 10+j);
  end loop;

  for j in 1 .. 10
  loop
    select scs_random.rand
      into i
      from dual;
    scs_test.testBool('rand = ' || i,20+j,i > 0);
  end loop;
end;
/
show errors