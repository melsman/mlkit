-- test suite for scs_user package
-- $Id$

set serveroutput on;

declare
  u1 integer;
  u2 integer;

  imp_id1 integer;
  
  n integer;

  procedure test_gen_passwd(
    email in varchar2,
    n in integer
  ) 
  is 
    pw varchar2(2000);
  begin
    pw := scs_user.gen_passwd(test_gen_passwd.email);
    scs_test.testBool('gen_passwd [' || pw || ']',
                      test_gen_passwd.n,
                      length(pw) = scs_math.min(length(email)+4,8));
  end test_gen_passwd;

begin
  scs_test.printl('[Testing scs-users...]');
  u1 := scs_user.new(password => scs_user.gen_passwd('nh@it.edu'),
                     salt => scs_random.rand_string(30),
                     email => 'nh@it.edu',
                     first_names => 'Niels', 
                     last_name => 'Hallenberg', 
                     security_id => '141148-NH 1',
                     modifying_user => scs_user.system);

  scs_test.testExn('Create Dublicate User', 1,
                   'declare
                      u integer;
                    begin
                      u := scs_user.new(user_id => ''' || u1 || ''',
                                        password => scs_user.gen_passwd(''nh@it.edu''),
                                        salt => scs_random.rand_string(30),
                                        email => ''nh@it.edu'',
                                        first_names => ''Niels'', 
                                        last_name => ''Hallenberg'', 
                                        security_id => ''141148-NH 1'', 
                                        modifying_user => scs_user.system);
                    end;','f');

  scs_test.testBool('gen_passwd [' || '' || ']', 1, scs_user.gen_passwd('') is null);
  test_gen_passwd('n',2);
  test_gen_passwd('nh',3);
  test_gen_passwd('nh@',4);
  test_gen_passwd('nh@it-c.dk',5);
  test_gen_passwd('nhnhnhnhnhnhnh@it-c.dk',6);

  scs_test.printl('[Testing imp_exact_match...]');
  /* 1 */
  /* Add a relation */
  scs_person_rel.add(u1,'MyTable',1);
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','','','','MyTable',1,scs_user.system);
  scs_test.testBool('imp_exact_match', 1, scs_user.imp_exact_match(imp_id1) = u1);

  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','','','','MyTable',2,scs_user.system);
  scs_test.testBool('imp_exact_match', 2, scs_user.imp_exact_match(imp_id1) is null);

  /* 2 */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','141148-NH 1','','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 3, scs_user.imp_exact_match(imp_id1) = u1);

  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','141148-NH*1','','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 4, scs_user.imp_exact_match(imp_id1) is null);

  /* 3 */
  /* Empty email should return null */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'Niels','Hallenberg','','','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 5, scs_user.imp_exact_match(imp_id1) is null);

  /* Empty norm_name should return null */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','','nh@it.edu','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 6, scs_user.imp_exact_match(imp_id1) is null);

  /* An exact email is not enough */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'NielsP','Hallenberg','','nh@it.edu','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 7, scs_user.imp_exact_match(imp_id1) is null);

  /* These should match */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'Niels','Hallenberg','','nh@it.edu','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 8, scs_user.imp_exact_match(imp_id1) = u1);

  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'Niels Peter','Hallenberg','','nh@it.edu','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 9, scs_user.imp_exact_match(imp_id1) = u1);

  scs_test.printl('[Testing imp_row...]');
  /* There is an exact match */
  /* first_names */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'Hans','','','','','MyTable',1,scs_user.system);
  scs_user.imp_row(imp_id1);
  scs_test.testBool('imp_row', 0, scs_person.last_name(u1) = 'Hallenberg');
  scs_test.testBool('imp_row', 1, scs_person.first_names(u1) = 'Hans');

  /* last_name */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','Hansen','','','','MyTable',1,scs_user.system);
  scs_user.imp_row(imp_id1);
  scs_test.testBool('imp_row', 2, scs_person.first_names(u1) = 'Hans');
  scs_test.testBool('imp_row', 3, scs_person.last_name(u1) = 'Hansen');

  /* security_id */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','211266-1212','','','MyTable',1,scs_user.system);
  scs_user.imp_row(imp_id1);
  scs_test.testBool('imp_row', 4, scs_person.first_names(u1) = 'Hans');
  scs_test.testBool('imp_row', 5, scs_person.last_name(u1) = 'Hansen');
  scs_test.testBool('imp_row', 6, scs_person.security_id(u1) = '211266-1212');

  /* email */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','','nh@itu.dk','','MyTable',1,scs_user.system);
  scs_user.imp_row(imp_id1);
  scs_test.testBool('imp_row', 7, scs_person.first_names(u1) = 'Hans');
  scs_test.testBool('imp_row', 8, scs_person.last_name(u1) = 'Hansen');
  scs_test.testBool('imp_row', 9, scs_person.security_id(u1) = '211266-1212');
  scs_test.testBool('imp_row', 10, scs_party.email(u1) = 'nh@itu.dk');

  /* url */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','','','http://my.company.com','MyTable',1,scs_user.system);
  scs_user.imp_row(imp_id1);
  scs_test.testBool('imp_row', 11, scs_person.first_names(u1) = 'Hans');
  scs_test.testBool('imp_row', 12, scs_person.last_name(u1) = 'Hansen');
  scs_test.testBool('imp_row', 13, scs_person.security_id(u1) = '211266-1212');
  scs_test.testBool('imp_row', 14, scs_party.email(u1) = 'nh@itu.dk');
  scs_test.testBool('imp_row', 15, scs_party.url(u1) = 'http://my.company.com');

  /* on_what_table and on_which_id */
  scs_test.testBool('imp_row', 16, scs_person_rel.exists_p(u1,'MyNewTable',4) = 'f');
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','211266-1212','','','MyNewTable',4,scs_user.system);
  scs_user.imp_row(imp_id1);
  scs_test.testBool('imp_row', 17, scs_person_rel.exists_p(u1,'MyNewTable',4) = 't');

  /* There is not an exact match and no normalised name */
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'Peter Hugo','Pedersen','','','','MyNewTable',5,scs_user.system);
  select count(*) 
    into n
    from scs_persons
   where scs_persons.first_names = 'Peter Hugo'
     and scs_persons.last_name = 'Pedersen'
     and scs_persons.deleted_p = 'f';
  scs_test.testBool('imp_row', 18, n = 0);
  scs_user.imp_row(imp_id1);
  select count(*) 
    into n
    from scs_persons
   where scs_persons.first_names = 'Peter Hugo'
     and scs_persons.last_name = 'Pedersen'
     and scs_persons.deleted_p = 'f';
  scs_test.testBool('imp_row', 19, n = 1);
  select person_id
    into u2
    from scs_persons
   where scs_persons.first_names = 'Peter Hugo'
     and scs_persons.last_name = 'Pedersen'
     and scs_persons.deleted_p = 'f';
  scs_test.testBool('imp_row', 20, scs_person_rel.exists_p(u2,'MyNewTable',5) = 't');

  /* There is not an exact match but at least one with a similar normalised name */

  /* Delete all import entries. */
  delete from scs_user_imports;
  select count(*)
    into n
    from scs_user_imports;
  scs_test.testBool('scs_user_imports empty',1,n=0);

  /* Delete test-user */
  scs_test.printl('[Testing Destroy...]');
  scs_test.testBool('Deleted_p',1,scs_user.deleted_p(u1) = 'f');
  scs_test.testUnit('Destroy',1,'
                     begin
                       scs_user.destroy(' || u1 || ');
                     end;');
  scs_test.testBool('Deleted_p',2,scs_user.deleted_p(u1) = 't');
  scs_test.testUnit('Destroy',2,'
                    begin
                      scs_user.destroy(' || u1 || ');
                    end;');
  scs_test.testUnit('Destroy',3,'
                     begin
                       scs_user.destroy(' || u2 || ');
                     end;');
end;
/
show errors
