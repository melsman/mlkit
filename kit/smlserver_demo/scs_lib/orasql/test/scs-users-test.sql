/* ======================================================================
   test suite for scs_user package

   $Id$

   History: 
   191102 Kennie Nybo Pontoppidan <kennie@it-c.dk> 
	  code review, added test cases and comments
   281002 Niels Hallenberg <nh@it.edu> created test suite
====================================================================== */

set serveroutput on;

create table tmp_table1( 
  passwd varchar(50) 
    constraint tmp_table1_passwd_pk primary key
);
commit;

declare
  uid1		integer;
  uid2		integer;
  tmp_uid	integer;

  invalid_uid	integer;

  v_screen_name	varchar2(100);
  v_email	varchar2(100);

  v_deleted_p1	char(1);
  v_deleted_p2	char(1);

  n integer;

  counter1_b	integer;
  counter1_a	integer;
  counter2_b	integer;
  counter2_a	integer;
  counter3_b	integer;
  counter3_a	integer;
  counter4_b	integer;
  counter4_a	integer;
  counter0_b	integer;
  counter0_a	integer;
  counter00_b	integer;
  counter00_a	integer;
  counter000_b	integer;
  counter000_a	integer;
  counter0000_b	integer;
  counter0000_a	integer;

  -- used to test the function 'scs_users.gen_passwd'
  procedure test_gen_passwd(
    pw_length		in integer,
    testcase		in integer
  ) 
  is 
    pw		varchar2(50);
    i		integer;
    pw_failed	exception;
  begin
    pw := scs_user.gen_passwd( pw_length );

    for i in 1..length( pw ) loop
      if substr( pw, i ) in ('0', 'O', '1', 'l') then
        raise pw_failed;
      end if;
    end loop;

    scs_test.testBool( 'gen_passwd [' || pw || ']',
                       test_gen_passwd.testcase, true );
  exception
    when pw_failed then
      scs_test.testBool( 'gen_passwd [' || pw || ']',
                       test_gen_passwd.testcase, false );
  end test_gen_passwd;

-- end of declare block
begin
  scs_test.printl( '------------------------' );
  scs_test.printl( 'testing scs_user package' );
  scs_test.printl( '------------------------' );

  select count(*) into counter0_b from scs_users;
  select count(*) into counter00_b from scs_user_preferences;
  select count(*) into counter000_b from scs_persons;
  select count(*) into counter0000_b from scs_parties;

  invalid_uid := scs.new_obj_id;

  scs_test.printl( 'testing function ''new'':' );
  select count(*) into counter1_b from scs_users;
  select count(*) into counter2_b from scs_persons;
  select count(*) into counter3_b from scs_parties;
  select count(*) into counter4_b from scs_user_preferences;
  uid1 := scs.new_obj_id;
  v_email := 'nh@it.edu' || to_char(uid1);
  v_screen_name := 'niels' || uid1;
  uid1 := scs_user.new( 
    user_id => uid1,
    password => scs_user.gen_passwd( 8 ),
    salt => scs_random.rand_string(30),
    screen_name => v_screen_name,
    email => v_email,
    first_names => 'Niels', 
    last_name => 'Hallenberg', 
    security_id => uid,
    modifying_user => scs_user.system
  );

  select count(*) into counter1_a from scs_users;
  select count(*) into counter2_a from scs_persons;
  select count(*) into counter3_a from scs_parties;
  select count(*) into counter4_a from scs_user_preferences;
  scs_test.testBool( 'new', 1, 
        counter1_a = counter1_b+1 AND counter2_a = counter2_b+1 AND
        counter3_a = counter3_b+1 AND counter4_a = counter4_b+1 );
  select user_id into tmp_uid 
    from scs_users us, scs_persons per, scs_parties par
   where us.user_id = per.person_id
     and per.person_id = par.party_id
     and par.email = 'nh@it.edu' || uid1;
  scs_test.testBool( 'new', 2, uid1 = tmp_uid );

  -- illegal values: an user_id is supplied and there exists an entry in scs_persons 
  --		     with this id as person_id
  scs_test.testExn( 'new', 3, '
    declare
      uid	integer;
    begin
      uid := scs_user.new(
        user_id => ' || to_char(uid1) || ',
        password => scs_user.gen_passwd( 8 ),
        salt => scs_random.rand_string(30),
        email => ''' || to_char(uid1) || 'nh@it.edu'',
        first_names => ''Niels'', 
        last_name => ''Hallenberg'', 
        security_id => ''141148-NH 1'',
        modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: an email is supplied that exists in the table scs_party
  scs_test.testExn( 'new', 4, '
    declare
      uid	integer;
    begin
      uid := scs_user.new(
        password => scs_user.gen_passwd( 8 ),
        salt => scs_random.rand_string(30),
        email => ''' || v_email || ''',
        first_names => ''Niels'', 
        last_name => ''Hallenberg'', 
        security_id => ''141148-NH 1'',
        modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: a screen_name is supplied that exists in the table scs_users
  scs_test.testExn( 'new', 5, '
    declare
      uid	integer;
    begin
      uid := scs_user.new(
        password => scs_user.gen_passwd( 8 ),
        salt => scs_random.rand_string(30),
        screen_name => ''' || v_screen_name || ''',
        email => scs_random.rand_string(30),
        first_names => ''Niels'', 
        last_name => ''Hallenberg'', 
        security_id => ''141148-NH 1'',
        modifying_user => scs_user.system);
    end;', 'f' );

  scs_test.printl( 'testing ''gen_passwd'':' );
  test_gen_passwd(  1, 1 );
  test_gen_passwd( 10, 2);
  test_gen_passwd( 50, 3);
  scs_test.testBool( 'gen_passwd', 4, scs_user.gen_passwd( -10 ) is null );
  scs_test.testBool( 'gen_passwd', 5, scs_user.gen_passwd( 0 ) is null );
  scs_test.testBool( 'gen_passwd', 6, scs_user.gen_passwd( 51 ) is null );
  scs_test.testBool( 'gen_passwd', 7, scs_user.gen_passwd( 100 ) is null );
  scs_test.testUnit( 'gen_passwd', 8, '
    declare
      i		integer;
    begin
      for i in 1..1000 loop
        insert into tmp_table1( passwd ) values ( scs_user.gen_passwd(8) );
      end loop;
    end;' );

  scs_test.printl( 'testing function ''system'':' );
  select person_id into tmp_uid from scs_persons
   where first_names = 'Site-wide SCS Administrator';
  scs_test.testBool( 'system', 1, tmp_uid = scs_user.system );  

  -- Delete all import entries. 
  delete from scs_user_imports;
  select count(*)
    into n
    from scs_user_imports;
  scs_test.testBool('scs_user_imports empty',1,n=0);

  scs_test.printl( 'testing procedure ''destroy'' and function ''deleted_p'':' );
  scs_test.testBool( 'deleted_p', 1, scs_user.deleted_p(uid1) = 'f' );
  -- destroying existing non-deleted user
  scs_test.testUnit( 'destroy', 1, '
                     begin
                       scs_user.destroy(' || uid1 || ');
                     end;');
  scs_test.testBool('deleted_p', 2, scs_user.deleted_p(uid1) = 't' );
  scs_test.testBool('destroy', 2, scs_user.deleted_p(uid1) = 't' );
  select screen_name into v_screen_name
    from scs_users
   where user_id = uid1;
  scs_test.testBool( 'destroy', 3, v_screen_name = scs.invalidate_field( 'niels' || uid1, 100, uid1 ) );
  select per.deleted_p, par.deleted_p into v_deleted_p1, v_deleted_p2 
    from scs_persons per, scs_parties par
   where per.person_id = par.party_id
     and person_id = uid1;
  scs_test.testBool( 'destroy', 4, v_deleted_p1 = 't' AND v_deleted_p2 = 't' );
  -- destroying existing deleted user multiple times
  scs_test.testUnit( 'destroy', 5, '
    declare
      i integer;
    begin
      for i in 1..1000 loop
        scs_user.destroy(' || uid1 || ');
      end loop;
    end;' );

  -- destroying non-existing user
  scs_test.testUnit( 'destroy', 6, '
                     begin
                       scs_user.destroy(' || invalid_uid || ');
                     end;');
  -- illegal user_id
  scs_test.testBool( 'deleted_p', 3, scs_user.deleted_p( invalid_uid ) is null );

  scs_test.printl( 'testing function ''language_pref'':' );
  -- legal user_id
  scs_test.testBool( 'language_pref', 1, scs_user.language_pref( uid1 ) = 'en' );
  -- illegal user_id
  scs_test.testBool( 'language_pref', 2, 
    scs_user.language_pref( invalid_uid ) is null  );

  -- cleaning up
  scs_test.printl( 'cleaning up...' );
  delete scs_user_preferences where user_id in ( uid1 );
  delete scs_users where user_id in ( uid1 );
  delete scs_persons where person_id in ( uid1 );
  delete scs_parties where party_id in  ( uid1 );

  select count(*) into counter0_a from scs_users;
  select count(*) into counter00_a from scs_user_preferences;
  select count(*) into counter000_a from scs_persons;
  select count(*) into counter0000_a from scs_parties;

  scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
  scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
  scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
  scs_test.testBool( 'garbage check', 3, counter0000_b = counter0000_a );
end;
/
show errors

drop table tmp_table1;
commit;

/
show errors

declare
  uid1			integer;
  uid2			integer;
  uid3			integer;
  uid4			integer;
  uid5			integer;

  invalid_id		integer;

  imp_id1		integer;
  imp_id2		integer;
  imp_id3		integer;
  imp_id4		integer;
  imp_id5		integer;
  imp_id6		integer;
  imp_id7		integer;
  imp_id8		integer;
  imp_id9		integer;
  imp_id10		integer;
  imp_id11		integer;
  imp_id12		integer;
  imp_id13		integer;
  imp_id14		integer;
  imp_id15		integer;
  imp_id16		integer;
  imp_id17		integer;
  imp_id18		integer;
  imp_id19		integer;

  n			integer;

  person_r		scs_persons%ROWTYPE;
  party_r		scs_parties%ROWTYPE;

  assert_b		boolean;
  assert_a		boolean;

  counter1_b		integer;
  counter1_a		integer;
  counter0_b		integer;
  counter0_a		integer;
  counter00_b		integer;
  counter00_a		integer;
  counter000_b		integer;
  counter000_a		integer;
  counter0000_b		integer;
  counter0000_a		integer;
  counter00000_b	integer;
  counter00000_a	integer;
  counter000000_b	integer;
  counter000000_a	integer;
begin
  scs_test.printl( '------------------------------------------------' );
  scs_test.printl( 'testing import functionality in scs_user package' );
  scs_test.printl( '------------------------------------------------' );

  select count(*) into counter0_b from scs_users;
  select count(*) into counter00_b from scs_user_preferences;
  select count(*) into counter000_b from scs_persons;
  select count(*) into counter0000_b from scs_person_rels;
  select count(*) into counter00000_b from scs_parties;
  select count(*) into counter000000_b from scs_user_imports;

  uid1 := scs.new_obj_id;
  uid1 := scs_user.new(
    user_id => uid1,
    password => scs_user.gen_passwd( 8 ),
    salt => scs_random.rand_string(30),
    screen_name => 'niels' || uid1,
    email => 'nh@it.edu' || uid1,
    first_names => 'Niels', 
    last_name => 'Hallenberg', 
    security_id => uid1,
    modifying_user => scs_user.system 
  );

  select scs_user_imports_id_seq.nextval into invalid_id from dual;

  scs_test.printl( 'testing function ''imp_exact_match'':' );
  -- Add a relation 
  scs_person_rel.add(uid1,'MyTable',1);

  -- 1 - match on (table,id)
  select scs_user_imports_id_seq.nextval
    into imp_id1
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id1,'','','','','','MyTable',1,scs_user.system);
  scs_test.testBool('imp_exact_match', 1, scs_user.imp_exact_match(imp_id1) = uid1);

  -- 1 - match on (table,id): non-existing person record with this (table,id)-pair
  select scs_user_imports_id_seq.nextval
    into imp_id2
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id2,'','','','','','MyTable',2,scs_user.system);
  scs_test.testBool( 'imp_exact_match', 2, 
    scs_user.imp_exact_match(imp_id2) is null );

  -- 2 - match on security id
  select scs_user_imports_id_seq.nextval
    into imp_id3
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id3,'','',uid1,'','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 3, 
    scs_user.imp_exact_match(imp_id3) = uid1);

  -- 2 - match on security id: non-existing person record with this security id
  select scs_user_imports_id_seq.nextval
    into imp_id4
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id4,'','','141148-NH*1','','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 4, scs_user.imp_exact_match(imp_id4) is null);

  -- 3 
  -- Empty email should return null 
  select scs_user_imports_id_seq.nextval
    into imp_id5
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id5,'Niels','Hallenberg','','','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 5, scs_user.imp_exact_match(imp_id5) is null);

  -- Empty norm_name should return null 
  select scs_user_imports_id_seq.nextval
    into imp_id6
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id6,'','','','nh@it.edu','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 6, scs_user.imp_exact_match(imp_id6) is null);

  -- An exact match on email and not on normalised name is not enough 
  select scs_user_imports_id_seq.nextval
    into imp_id7
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id7,'NielsP','Hallenberg','','nh@it.edu','','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 7, scs_user.imp_exact_match(imp_id7) is null);

  -- This should match: normalised name and email matches
  select scs_user_imports_id_seq.nextval
    into imp_id8
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id8,'Niels','Hallenberg','','nh@it.edu'||uid1,'','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 8, scs_user.imp_exact_match(imp_id8) = uid1);
  
  -- This should match: normalised name and email matches
  select scs_user_imports_id_seq.nextval
    into imp_id9
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id9,'Niels Peter','Hallenberg','','nh@it.edu'||uid1,'','',null,scs_user.system);
  scs_test.testBool('imp_exact_match', 9, scs_user.imp_exact_match(imp_id9) = uid1);
  
  -- two users uid3, uid4 with same security id
  uid3 := scs.new_obj_id;
  uid3 := scs_user.new(
    user_id => uid3,
    password => scs_user.gen_passwd( 8 ),
    salt => scs_random.rand_string(30),
    screen_name => 'niels' || uid3,
    email => 'nh@it.edu' || uid3,
    first_names => 'Niels', 
    last_name => 'Hallenberg', 
    security_id => uid3,
    modifying_user => scs_user.system 
  );
  uid4 := scs.new_obj_id;
  uid4 := scs_user.new(
    user_id => uid4,
    password => scs_user.gen_passwd( 8 ),
    salt => scs_random.rand_string(30),
    screen_name => 'niels' || uid4,
    email => 'nh@it.edu' || uid4,
    first_names => 'Niels', 
    last_name => 'Hallenberg', 
    security_id => uid3,
    modifying_user => scs_user.system 
  );
  select scs_user_imports_id_seq.nextval
    into imp_id17
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id17,'Niels Peter','Hallenberg',uid3,'','','',null,scs_user.system);
  scs_test.testBool( 'imp_exact_match', 10, 
    scs_user.imp_exact_match(imp_id17) is null );

  scs_test.printl( 'testing procedure ''imp_row_into_user'':' );

  uid5 := scs.new_obj_id;
  uid5 := scs_user.new(
    user_id => uid5,
    password => scs_user.gen_passwd( 8 ),
    salt => scs_random.rand_string(30),
    screen_name => 'niels' || uid5,
    email => 'nh@it.edu' || uid5,
    first_names => 'Niels', 
    last_name => 'Hallenberg', 
    security_id => uid5,
    modifying_user => scs_user.system 
  );
  select scs_user_imports_id_seq.nextval
    into imp_id18
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id18,'Kurt','von der Übergeek',uid4,'kvdu','permanent','',null,scs_user.system);
  scs_user.imp_row_into_user(
    user_imp_id => imp_id18,
    user_id	=> uid5
  );
  select * into person_r from scs_persons where person_id = uid5;
  select * into party_r from scs_parties where party_id = uid5;
  scs_test.testBool( 'imp_row_into_user', 1, 
    person_r.first_names = 'Kurt' AND
    person_r.last_name  = 'von der Übergeek' AND
    person_r.security_id = uid4 AND
    party_r.email	 = 'kvdu'
  );
  scs_test.testBool( 'imp_row_into_user', 2, 
    party_r.url	 = 'permanent'
  );

  select scs_user_imports_id_seq.nextval
  into imp_id19
  from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id19,'','','','','not changing','',null,scs_user.system);
  scs_user.imp_row_into_user(
    user_imp_id => imp_id19,
    user_id	=> uid5
  );
  select * into party_r from scs_parties where party_id = uid5;
  scs_test.testBool( 'imp_row_into_user', 3, 
    party_r.url	 = 'permanent'
  );

  -- illegal user_id
  scs_test.testUnit( 'imp_row_into_user', 4, '
    begin
      scs_user.imp_row_into_user(
        user_imp_id => ' || imp_id19 || ',
	user_id	=> ' || invalid_id || '
      );  
    end;' );

  -- illegal imp_id
  scs_test.testUnit( 'imp_row_into_user', 5, '
    begin
      scs_user.imp_row_into_user(
        user_imp_id => ' || invalid_id || ',
	user_id	=> ' || uid5 || '
      );
    end;' );

  -- both id's illegal
  scs_test.testUnit( 'imp_row_into_user', 6, '
    begin
      scs_user.imp_row_into_user(
        user_imp_id => ' || invalid_id || ',
	user_id	=> ' || invalid_id || '
      );  
    end;' );

  scs_test.printl('testing procedure ''imp_row'':');
  -- There is an exact match on (table,id)-pair: new first_names 
  select scs_user_imports_id_seq.nextval
    into imp_id10
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id10,'Hans','','','','','MyTable',1,scs_user.system);
  scs_user.imp_row( imp_id10 );
  scs_test.testBool('imp_row', 0, 
    scs_person.last_name(uid1) = 'Hallenberg' AND
    scs_person.first_names(uid1) = 'Hans' );

  -- There is an exact match on (table,id)-pair: new last_name 
  select scs_user_imports_id_seq.nextval
    into imp_id11
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id11,'','Hansen','','','','MyTable',1,scs_user.system);
  scs_user.imp_row( imp_id11 );
  scs_test.testBool( 'imp_row', 1, 
    scs_person.first_names(uid1) = 'Hans' AND
    scs_person.last_name(uid1) = 'Hansen' );

  -- There is an exact match on (table,id)-pair: new security_id
  select scs_user_imports_id_seq.nextval
    into imp_id12
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id12,'','','211266-1212','','','MyTable',1,scs_user.system);
  scs_user.imp_row( imp_id12 );
  scs_test.testBool( 'imp_row', 2, 
    scs_person.first_names(uid1) = 'Hans' AND
    scs_person.last_name(uid1) = 'Hansen' AND
    scs_person.security_id(uid1) = '211266-1212' );

  -- There is an exact match on (table,id)-pair: new email 
  select scs_user_imports_id_seq.nextval
    into imp_id13
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id13,'','','','nh@itu.dk','','MyTable',1,scs_user.system);
  scs_user.imp_row( imp_id13 );
  scs_test.testBool('imp_row', 3, 
    scs_person.first_names(uid1) = 'Hans' AND
    scs_person.last_name(uid1) = 'Hansen' AND
    scs_person.security_id(uid1) = '211266-1212' AND
    scs_party.email(uid1) = 'nh@itu.dk' );

  -- There is an exact match on (table,id)-pair: new url 
  select scs_user_imports_id_seq.nextval
    into imp_id14
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id14,'','','','','http://my.company.com','MyTable',1,scs_user.system);
  scs_user.imp_row( imp_id14 );
  scs_test.testBool( 'imp_row', 4, 
    scs_person.first_names(uid1) = 'Hans' AND
    scs_person.last_name(uid1) = 'Hansen' AND
    scs_person.security_id(uid1) = '211266-1212' AND
    scs_party.email(uid1) = 'nh@itu.dk' AND
    scs_party.url(uid1) = 'http://my.company.com' );

  -- There is an exact match on security-id: new (table,id)-pair
  assert_b := scs_person_rel.exists_p(uid1,'MyNewTable',4) = 'f';
  select scs_user_imports_id_seq.nextval
    into imp_id15
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id15,'','','211266-1212','','','MyNewTable',4,scs_user.system);
  scs_user.imp_row(imp_id15);
  assert_a := scs_person_rel.exists_p(uid1,'MyNewTable',4) = 't';
  scs_test.testBool('imp_row', 5, assert_b AND assert_a );

  -- There is not an exact match and no normalised name in scs_persons
  select scs_user_imports_id_seq.nextval
    into imp_id16
    from dual;
  insert into scs_user_imports 
    (user_imp_id,first_names,last_name,security_id,email,url,on_what_table,on_which_id,modifying_user)
  values (imp_id16,'Peter Hugo',to_char(imp_id16),'','','','MyNewTable',imp_id16,scs_user.system);
  select count(*) 
    into counter1_b
    from scs_persons
   where scs_persons.first_names = 'Peter Hugo'
     and scs_persons.last_name = to_char(imp_id16)
     and scs_persons.deleted_p = 'f';

  scs_user.imp_row(imp_id16);

  select count(*) 
    into counter1_a
    from scs_persons
   where scs_persons.first_names = 'Peter Hugo'
     and scs_persons.last_name = to_char(imp_id16)
     and scs_persons.deleted_p = 'f';
  scs_test.testBool('imp_row', 6, counter1_a = counter1_b + 1 );
  
  select person_id
    into uid2
    from scs_persons
   where scs_persons.first_names = 'Peter Hugo'
     and scs_persons.last_name = to_char(imp_id16)
     and scs_persons.deleted_p = 'f';

  scs_test.testBool( 'imp_row', 7, 
    scs_person_rel.exists_p(uid2,'MyNewTable',imp_id16) = 't' );

  -- illegal imp_id
  scs_test.testUnit( 'imp_row', 8, '
    begin
      scs_user.imp_row( ' || invalid_id || ');
    end;' );


  -- cleaning up
  scs_test.printl( 'cleaning up...' );
  delete scs_user_imports where user_imp_id in( 
    imp_id1,  imp_id2,  imp_id3,  imp_id4,
    imp_id5,  imp_id6,  imp_id7,  imp_id8,
    imp_id9,  imp_id10, imp_id11, imp_id12,
    imp_id13, imp_id14, imp_id15, imp_id16,
    imp_id17, imp_id18 );
  delete scs_user_preferences where user_id in ( uid1, uid2, uid3, uid4, uid5 );
  delete scs_users where user_id in ( uid1, uid2, uid3, uid4, uid5 );
  delete scs_person_rels where person_id in ( uid1, uid2, uid3, uid4, uid5 );
  delete scs_persons where person_id in ( uid1, uid2, uid3, uid4, uid5 );
  delete scs_parties where party_id in  ( uid1, uid2, uid3, uid4, uid5 );

  select count(*) into counter0_a from scs_users;
  select count(*) into counter00_a from scs_user_preferences;
  select count(*) into counter000_a from scs_persons;
  select count(*) into counter0000_a from scs_person_rels;
  select count(*) into counter00000_a from scs_parties;
  select count(*) into counter000000_a from scs_user_imports;

  scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
  scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
  scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
  scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );
  scs_test.testBool( 'garbage check', 5, counter00000_b = counter00000_a );
  scs_test.testBool( 'garbage check', 6, counter000000_b = counter000000_a );

exception
  when others then
    scs_test.printl( 'an unknown error occured' );      
    -- cleaning up
    scs_test.printl( 'cleaning up...' );
    delete scs_user_imports where user_imp_id in( 
      imp_id1,  imp_id2,  imp_id3,  imp_id4,
      imp_id5,  imp_id6,  imp_id7,  imp_id8,
      imp_id9,  imp_id10, imp_id11, imp_id12,
      imp_id13, imp_id14, imp_id15, imp_id16,
      imp_id17, imp_id18 );
    delete scs_user_preferences where user_id in ( uid1, uid2, uid3, uid4, uid5 );
    delete scs_users where user_id in ( uid1, uid2, uid3, uid4, uid5 );
    delete scs_person_rels where person_id in ( uid1, uid2, uid3, uid4, uid5 );
    delete scs_persons where person_id in ( uid1, uid2, uid3, uid4, uid5 );
    delete scs_parties where party_id in  ( uid1, uid2, uid3, uid4, uid5 );
    
    select count(*) into counter0_a from scs_users;
    select count(*) into counter00_a from scs_user_preferences;
    select count(*) into counter000_a from scs_persons;
    select count(*) into counter0000_a from scs_person_rels;
    select count(*) into counter00000_a from scs_parties;
    select count(*) into counter000000_a from scs_user_imports;
    
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
    scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
    scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
    scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );
    scs_test.testBool( 'garbage check', 5, counter00000_b = counter00000_a );
    scs_test.testBool( 'garbage check', 6, counter000000_b = counter000000_a );
end;
/ 
show errors
