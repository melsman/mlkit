-- $id$

set serveroutput on

declare
  pid1		integer;
  pid2		integer;
  pid3		integer;
  illegal_pid	integer;
  n		integer;

  counter1_b	integer;
  counter1_a	integer;
  counter2_b	integer;
  counter2_a	integer;
  counter0_b	integer;
  counter0_a	integer;
  counter00_b	integer;
  counter00_a	integer;
begin
  scs_test.printl( '-------------------------------' );
  scs_test.printl( 'testing scs_person package' );
  scs_test.printl( '-------------------------------' );
  select count(*) into counter0_b from scs_persons;
  select count(*) into counter00_b from scs_parties;

  illegal_pid := scs.new_obj_id;

  scs_test.printl( 'testing function ''new'':' );
  select count(*) into counter2_b from scs_persons;
  -- legal values
  pid1 := scs.new_obj_id;
  pid1 := scs_person.new( 
    person_id => pid1,
    first_names	=> 'Niels', 
    last_name	=> 'Hallenberg', 
    email		=> to_char(pid1) || 'nh@it-c.dk',
    modifying_user => scs_user.system );

  select count(*) into counter2_a from scs_persons;
  scs_test.testBool( 'new', 1, counter2_a = counter2_b + 1 );
  -- create dublicate Person
  scs_test.testExn( 'new', 2, '
    declare
      p integer;
    begin
      p := scs_person.new( person_id	  => ' || to_char(pid1) || ',
                           first_names	  => ''Niels'', 
                           last_name	  => ''Hallenberg'', 
                           modifying_user => scs_user.system );
    end;', 'f' );

  -- create person with email already in use
  pid2 := scs.new_obj_id;
  scs_test.testExn( 'new', 3, '
    declare
      p integer;
    begin
      p := scs_person.new( 
        person_id => ' || to_char(pid2) || ',
        email	  => ''' || to_char(pid1) || 'nh@it-c.dk'',
        first_names	  => ''Niels'', 
	last_name	  => ''Hallenberg'', 
	modifying_user => scs_user.system );
    end;', 'f' );

  scs_test.printl('testing procedures ''destroy'' and ''deleted_p'':');  
  scs_test.testBool('deleted_p',1,scs_person.deleted_p(pid1) = 'f');
  scs_test.testUnit( 'destroy', 1, 'begin scs_person.destroy(' || pid1 || '); end;');
  select count(*) into counter1_a 
  from scs_person_rels
  where person_id = pid1;
  scs_test.testBool('deleted_p',2,scs_person.deleted_p(pid1) = 't');
  scs_test.testBool( 'destroy', 2, scs_person.deleted_p(pid1) = 't' );


  -- repeated destroys
  scs_test.testUnit('destroy', 3, '
    declare
      i integer;
    begin
      for i in 1..1000 loop
        scs_person.destroy(' || pid1 || ');
      end loop;
    end;');

  -- non-existing person 
  scs_test.testBool( 'deleted_p', 3, 
    scs_person.deleted_p( illegal_pid) is null );

  -- non-existing person 
  scs_test.testUnit('destroy', 4, '
    begin 
      scs_person.destroy(  ' || to_char(illegal_pid) || ' ); 
    end;'); 

  scs_test.printl( 'testing procedure ''new'' once more:' );
  -- recreate a person after he was destroyed
  pid3 := scs.new_obj_id;
  scs_test.testUnit( 'new', 4, '
    declare
      tmp integer;
    begin
      tmp := scs_person.new( 
	       person_id => ' || to_char(pid3) || ',
	       first_names	=> ''Niels'', 
	       last_name	=> ''Hallenberg'', 
	       email		=> ''' || to_char(pid1) || 'nh@it-c.dk'',
	       security_id	=> ''000000-0000'',
	       modifying_user => scs_user.system );
    end;' );

  scs_test.printl( 
    'testing functions ''first_names'', ''last_names'', ''name'':' );
  scs_test.testBool( 'first_names', 1, scs_person.first_names(pid3) = 'Niels');
  scs_test.testBool( 'first_names', 2, 
    scs_person.first_names( illegal_pid ) is null );

  scs_test.testBool( 'last_name', 1, 
    scs_person.last_name(pid3) = 'Hallenberg' );
  scs_test.testBool( 'last_name', 2, 
    scs_person.last_name( illegal_pid ) is null );

  scs_test.testBool( 'name', 1, scs_person.name(pid3) = 'Niels Hallenberg' );
  scs_test.testBool( 'name', 2, scs_person.name( illegal_pid ) is null );

  scs_test.printl( 'testing function ''security_id'':' );
  scs_test.testBool( 'security_id', 1, scs_person.security_id(pid3) = '000000-0000' );
  scs_test.testBool( 'security_id', 2, scs_person.security_id( illegal_pid ) is null );

  scs_test.printl( 'testing functions ''normalize_name'':' );

  scs_test.testBool( 'normalise_name (2 parameter version)', 1, 
    scs_person.norm_name('Niels','Hallenberg') = 'nielshallenberg' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 2, 
    scs_person.norm_name('','Hallenberg') = 'hallenberg' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 3, 
    scs_person.norm_name(' ','Hallenberg') = 'hallenberg' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 4, 
    scs_person.norm_name('Niels','') = 'niels' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 5, 
    scs_person.norm_name('Niels',' ') = 'niels' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 6, 
    scs_person.norm_name(' niels ','Hallenberg') = 'nielshallenberg' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 7, 
    scs_person.norm_name(' niels igen','Hallenberg') = 'nielshallenberg' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 8, 
    scs_person.norm_name('niels','Hallenberg hal') = 'nielshallenberghal' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 9, 
    scs_person.norm_name('niels','') = 'niels' );
  scs_test.testBool( 'normalise_name (2 parameter version)', 10, 
    scs_person.norm_name(' ','') is null );
  scs_test.testBool( 'normalise_name (2 parameter version)', 11, 
      scs_person.norm_name(' niels henrik pedersen','Hallenberg') 
    = 'nielshallenberg' );
  scs_test.testBool('normalise_name (2 parameter version)', 12, 
      scs_person.norm_name(' nie ls     henri    k pedersen   ','Hallenberg')
    = 'niehallenberg');

  scs_test.testBool( 'normalise_name (1 parameter version)', 1, 
    scs_person.norm_name(pid3) = 'nielshallenberg' );
  scs_test.testBool( 'normalise_name (1 parameter version)', 2, 
    scs_person.norm_name(illegal_pid) is null );

  -- normalised name exist
  scs_test.testBool( 'norm_name_exists_p', 1, 
    scs_person.norm_name_exists_p( 'nielshallenberg' ) = 't' );
  -- normalised name doesn't exist
  scs_test.testBool( 'norm_name_exists_p', 1, 
    scs_person.norm_name_exists_p( '42grimmeheste' ) = 'f');

  -- cleaning up
  scs_test.printl( 'cleaning up...' );
  delete scs_persons where person_id in ( pid1, pid2, pid3 );
  delete scs_parties where party_id in  ( pid1, pid2, pid3 );
  select count(*) into counter0_a from scs_persons;
  select count(*) into counter00_a from scs_parties;
  scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
  scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
end; --of scs_person test
/
show errors

declare
  pid1		integer;
  illegal_pid	integer;

  counter0_b	integer;
  counter0_a	integer;
  counter00_b	integer;
  counter00_a	integer;
  counter000_b	integer;
  counter000_a	integer;
  counter1_b	integer;
  counter1_a	integer;
begin
  select count(*) into counter0_b from scs_persons;
  select count(*) into counter00_b from scs_parties;
  select count(*) into counter000_b from scs_person_rels;

  pid1 := scs.new_obj_id;
  pid1 := scs_person.new( 
    person_id => pid1,
    first_names	=> 'Niels', 
    last_name	=> 'Hallenberg', 
    email		=> to_char(pid1) || 'nh@it-c.dk',
    modifying_user => scs_user.system );

  illegal_pid := scs.new_obj_id;

  scs_test.printl( '-------------------------------' );
  scs_test.printl( 'testing scs_person_rel package' );
  scs_test.printl( '-------------------------------' );

  select count(*) into counter1_b from scs_person_rels;
  scs_test.printl( 'testing procedure ''add'':' );
  scs_test.testUnit( 'add', 1,'
    begin 
      scs_person_rel.add(' || pid1 || ',''MyTable'',1); 
    end;' );
  scs_test.testUnit( 'add', 2,'
    begin 
      scs_person_rel.add(' || pid1 || ',''MyTable2'',1); 
    end;' );
  -- Duplicate index 100 times
  scs_test.testUnit( 'add', 3,'
    declare
      i integer;
    begin 
      for i in 1..100 loop
        scs_person_rel.add(' || pid1 || ',''MyTable'',1); 
      end loop;
    end;' );
  scs_test.testUnit( 'add', 4,'
    begin 
      scs_person_rel.add(' || pid1 || ',''MyTable2'',2); 
    end;' );
  -- Non existing person
  scs_test.testExn( 'add',5,'
    begin 
      scs_person_rel.add(42,''MyTable3'',2); 
    end;', 'f' ); 
  select count(*) into counter1_a from scs_person_rels;
  scs_test.testBool( 'add', 6, counter1_a = counter1_b + 3 );

  scs_test.printl( 'testting procedure ''delete'':' );
  select count(*) into counter1_b from scs_person_rels;
  scs_test.testUnit( 'delete',1,'begin scs_person_rel.delete(' || pid1 || ',''MyTable'',1); end;');
  scs_test.testUnit( 'delete',2,'begin scs_person_rel.delete(' || pid1 || ',''MyTable2'',1); end;');
  -- The next one is already deleted 
  scs_test.testUnit( 'delete',3,'begin scs_person_rel.delete(' || pid1 || ',''MyTable'',1); end;');   
  scs_test.testUnit( 'delete',4,'begin scs_person_rel.delete(' || pid1 || ',''MyTable2'',2); end;');
  -- Non existing person - should not fail
  scs_test.testUnit( 'delete',5,'begin scs_person_rel.delete(42,''MyTable2'',2); end;'); 
  select count(*) into counter1_a from scs_person_rels;
  scs_test.testBool( 'delete', 6, counter1_a = counter1_b -3 );

  scs_test.printl( 'testing procedure ''delete_all'':' );
  scs_person_rel.add( pid1, 'MyTable', 1 );
  scs_person_rel.add( pid1, 'MyTable2', 1 );
  select count(*) into counter1_b 
    from scs_person_rels
   where person_id = pid1;
  scs_test.testUnit('delete_all',1,'begin scs_person_rel.delete_all(' || pid1 || '); end;');
  select count(*) into counter1_a
    from scs_person_rels
   where person_id = pid1;
  scs_test.testBool( 'delete_all', 2, counter1_a = counter1_b - 2 );
  -- no rows to delete
  scs_test.testUnit( 'delete_all', 3,'begin scs_person_rel.delete_all(' || pid1 || '); end;');
  -- Non existing person
  scs_test.testUnit('delete_all',3,'
    begin 
      scs_person_rel.delete_all( ' || to_char(illegal_pid) || ' ); 
    end;'); 

  scs_test.printl( 'testing function ''exists_p'':' );
  scs_person_rel.add( pid1, 'MyTable', 1 ); 
  -- (person_id, on_what_table, on_which_id) exist
  scs_test.testBool( 'exists_p', 1, 
    scs_person_rel.exists_p( pid1, 'MyTable', 1 ) = 't' );
  -- non-existing (person_id, on_what_table, on_which_id)
  scs_test.testBool( 'exists_p', 1, 
    scs_person_rel.exists_p( illegal_pid, null, null ) = 'f' );

  scs_test.printl( 'cleaning up...' );
  delete scs_person_rels where person_id in ( pid1 );
  delete scs_persons where person_id in ( pid1 );
  delete scs_parties where party_id in  ( pid1 );

  select count(*) into counter0_a from scs_persons;
  select count(*) into counter00_a from scs_parties;
  select count(*) into counter000_a from scs_person_rels;

  scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
  scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
  scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
end; --of scs_person_rel test
/ 
show errors
