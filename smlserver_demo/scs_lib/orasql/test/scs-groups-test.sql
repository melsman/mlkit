/* ======================================================================
   test suite for scs-group packages

   $ID$

   History: 
   271102 Kennie Nybo Pontoppidan <kennie@it-c.dk> 
	  code review, rewrote testsuite, added test cases and comments
   161002 Niels Hallenberg <nh@it.edu> created test suite
====================================================================== */

-- This code is a modified version of the acs-groups-test module found
-- in openACS (www.openacs.org): files acs-groups-test.sql

set serveroutput on

-- logging details
column log_key format a20;
column message format a90;
column ancestor_rel_type format a15;
column container_grp format a15;
column grp format a15;
column party format a15;
set lines 100

-- procedure used in test of denormalised scs_grp_party_index
create or replace procedure print_scs_grp_party_index 
is
begin
  scs_log.notice('GT','| Group |      Party | Rel_id | Container | Ancestor |');
  for row in (select scs_group.name(grp_id) as grp, 
                     scs_party.email(party_id) as party, 
                     rel_id, 
                     scs_group.name(container_id) as container_grp,
                     ancestor_rel_type
                from scs_grp_party_index
               order by rel_id) loop
    scs_log.notice('GT','| ' || lpad(row.grp,5) || ' | ' || lpad(row.party,10) || ' | ' || 
                   lpad(row.rel_id,6) || ' | ' || 
                   lpad(row.container_grp,9) || ' | ' || lpad(row.ancestor_rel_type,8) );
  end loop;
end print_scs_grp_party_index;
/
show errors


-- test of package scs_grp_type
alter trigger scs_grp_types_def_grp_type_tr disable;

declare
  grp_type_id1		integer;
  grp_type_id2		integer;
  grp_type_id3		integer;
  grp_type_id4		integer;
  grp_type_id5		integer;
  grp_type_id6		integer;
  grp_type_id7		integer;
  grp_type_id8		integer;
  grp_type_id9		integer;
  grp_type_id10		integer;
  default_grp_type_id	integer;

  illegal_id		integer;

  rand_name1		varchar2(10);
  rand_name2		varchar2(10);
  rand_name3		varchar2(10);
  rand_name4		varchar2(10);
  rand_name5		varchar2(10);
  rand_name6		varchar2(10);

  counter1_b		integer;
  counter1_a		integer;
  counter0_b		integer;
  counter0_a		integer;

  clean_up		exception;
begin
  scs_test.printl( '-----------------------------' );
  scs_test.printl( 'testing scs_grp_type package' );
  scs_test.printl( '-----------------------------' );

  select count(*) into counter0_b from scs_grp_types;

  illegal_id := scs.new_obj_id;

  scs_test.printl( 'testing function ''new'':' );
  -- create with legal type_id
  grp_type_id1 := scs.new_obj_id;
  rand_name1 := scs_random.rand_string(10);
  grp_type_id1 := scs_grp_type.new (
    grp_type_id			=> grp_type_id1,
    grp_type			=> rand_name1,
    modifying_user		=> scs_user.system 
  );
  select count(*) into counter1_a 
    from scs_grp_types 
   where grp_type_id = grp_type_id1
     and grp_type = rand_name1;
  scs_test.testBool( 'new', 1, counter1_a = 1 );

  -- create without type_id, checking default values
  rand_name2 := scs_random.rand_string(10);
  grp_type_id2 := scs_grp_type.new (
    grp_type			=> rand_name2,
    modifying_user		=> scs_user.system 
  );
  select count(*) into counter1_a 
    from scs_grp_types 
   where grp_type_id = grp_type_id2
     and grp_type = rand_name2
     and default_join_policy = 'open'
     and default_grp_type = 'f'
     and modifying_user = scs_user.system;
  scs_test.testBool( 'new', 2, counter1_a = 1 );

  -- explicit join_policy: 'open'
  rand_name3 := scs_random.rand_string(10);
  grp_type_id3 := scs_grp_type.new (
    grp_type			=> rand_name3,
    default_join_policy		=> 'open',
    modifying_user		=> scs_user.system 
  );
  select count(*) into counter1_a 
    from scs_grp_types 
   where grp_type_id = grp_type_id3
     and grp_type = rand_name3
     and modifying_user = scs_user.system;
  scs_test.testBool( 'new', 3, counter1_a = 1 );

  -- explicit join_policy: 'needs approval'
  rand_name4 := scs_random.rand_string(10);
  grp_type_id4 := scs_grp_type.new (
    grp_type			=> rand_name4,
    default_join_policy		=> 'needs approval',
    modifying_user		=> scs_user.system 
  );
  select count(*) into counter1_a 
    from scs_grp_types 
   where grp_type_id = grp_type_id4
     and grp_type = rand_name4
     and modifying_user = scs_user.system;
  scs_test.testBool( 'new', 4, counter1_a = 1 );

  -- explicit join_policy: 'closed'
  rand_name5 := scs_random.rand_string(10);
  grp_type_id5 := scs_grp_type.new (
    grp_type			=> rand_name5,
    default_join_policy		=> 'closed',
    modifying_user		=> scs_user.system 
  );
  select count(*) into counter1_a 
    from scs_grp_types 
   where grp_type_id = grp_type_id5
     and grp_type = rand_name5
     and modifying_user = scs_user.system;
  scs_test.testBool( 'new', 5, counter1_a = 1 );

  -- illegal values: there exists a group type with id 'grp_type_id'
  scs_test.testExn( 'new', 6, '
    declare
      id integer;
    begin
      id := scs_grp_type.new(
	grp_type_id		=> ' || grp_type_id1 || ',        
        grp_type		=> scs_random.rand_string(10),
        modifying_user		=> scs_user.system 
      );
    end;', 'f' );

  -- illegal values: there exists a group type with name (grp_type) 'grp_type'
  scs_test.testExn( 'new', 7, '
    declare
      id integer;
    begin
      id := scs_grp_type.new(
        grp_type		=> ''' || rand_name1 || ''',
        modifying_user		=> scs_user.system 
      );
    end;', 'f' );

  -- illegal values: default_join_policy is not 
  --		     'open', 'needs approval' or 'closed'
  scs_test.testExn( 'new', 8, '
    declare
      id integer;
    begin
      id := scs_grp_type.new(
        grp_type		=> scs_random.rand_string(10),
	default_join_policy	=> scs_random.rand_string(10),
        modifying_user		=> scs_user.system 
      );
    end;', 'f' );

  -- illegal values: default_grp_type is not 't' or 'f' 
  scs_test.testExn( 'new', 9, '
    declare
      id integer;
    begin
      id := scs_grp_type.new(
        grp_type		=> scs_random.rand_string(10),
	default_grp_type	=> ''a'',
        modifying_user		=> scs_user.system 
      );
    end;', 'f' );

  -- illegal values: no user exists with user_id 'modifying_user'          
  scs_test.testExn( 'new', 10, '
    declare
      id integer;
    begin
      id := scs_grp_type.new(
        grp_type		=> scs_random.rand_string(10),
        modifying_user		=> ' || illegal_id ||'
      );
    end;', 'f' );

  scs_test.printl( 'testing function ''default_grp_type'':' );
  select grp_type_id into default_grp_type_id
    from scs_grp_types
   where  default_grp_type = 't';

  scs_test.testBool( 'default_grp_type', 1, 
    scs_grp_type.default_grp_type = default_grp_type_id );
  -- testing exception
   update scs_grp_types set default_grp_type = 'f';
  
   scs_test.testExn( 'default_grp_type', 2 , '
     declare
       id integer;
     begin
       id := scs_grp_type.default_grp_type;
     end;', 'f' );

  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );

    -- restore default group type
    update scs_grp_types set default_grp_type = 't' 
     where grp_type_id = default_grp_type_id;

    delete scs_grp_types where grp_type_id in 
      ( grp_type_id1, grp_type_id2, grp_type_id3, grp_type_id4, 
	grp_type_id5, grp_type_id6, grp_type_id7, grp_type_id8, 
	grp_type_id9, grp_type_id10  );
    select count(*) into counter0_a from scs_grp_types;
  
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );

  when others then
    scs_test.print( 'an error occured: ' );
    scs_test.printl( SQLERRM );
    goto clean;
end; --of test of package scs_grp_type block
/
show errors
alter trigger scs_grp_types_def_grp_type_tr enable;
--test of package scs_grp_type really ends here


-- test of scs_group package
declare
  grp_id		integer;
  grp_name		integer;
  rand_nameA		varchar(10);
  rand_name1		varchar(10);
  A			integer;
  B			integer;
  C			integer;
  D			integer;
  E			integer;
  F			integer;
  G			integer;
  grp_no_email_id	integer;

  illegal_id		integer;
  id			integer;

  grp_type_id1		integer;
  A_grp_name		varchar2(100);
  A_modUser		integer;
  B_grp_type_id		integer;
  C_email		varchar2(100);
  D_url			varchar2(200);
  E_join		varchar2(20);
  F_join		varchar2(20);
  G_join		varchar2(20);

  joe			integer;
  jane			integer;
  bob			integer;
  betty			integer;
  jack			integer;
  jill			integer;
  sven			integer;
  stacy			integer;

  A_rel_id_B		integer;
  B_rel_id_C		integer;
  B_rel_id_D		integer;
  A_rel_id_C		integer;
  C_rel_id_E		integer;
  C_rel_id_bob		integer;
  C_rel_id_jane		integer;
  D_rel_id_jane		integer;
  D_rel_id_joe		integer;
  B_rel_id_C_member	integer;
  E_rel_id_betty	integer;

--  n_rows		integer;

  party_id1		integer;
--  party_id2		integer;

  counter1_b		integer;
  counter1_a		integer;
  counter2_b		integer;
  counter2_a		integer;
  counter0_b		integer;
  counter0_a		integer;
  counter00_b		integer;
  counter00_a		integer;
  counter000_b		integer;
  counter000_a		integer;
  counter0000_b		integer;
  counter0000_a		integer;

  clean_up		exception;
begin
  scs_test.printl( '--------------------------' );
  scs_test.printl( 'testing scs_groups package' );
  scs_test.printl( '--------------------------' );

  select count(*) into counter0_b from scs_groups;
  select count(*) into counter00_b from scs_grp_types;
  select count(*) into counter000_b from scs_parties;
  select count(*) into counter0000_b from scs_users;

  illegal_id := scs.new_obj_id;

  scs_test.printl( 'testing function ''new'':' );
  -- default values
  select count(*) into counter1_b from scs_groups;
  select count(*) into counter2_b from scs_parties;
  rand_nameA := scs_random.rand_string(10);
  grp_no_email_id := scs_group.new( 
    grp_name	     => rand_nameA, 
    modifying_user   => scs_user.system );
  select count(*) into counter1_a from scs_groups;
  select count(*) into counter2_a from scs_parties;
  select grp_name, modifying_user into A_grp_name, A_modUser
    from scs_groups
   where grp_id = grp_no_email_id;

  scs_test.testBool( 'new', 1, 
    counter1_a = counter1_b + 1 AND
    counter2_a = counter2_b + 1 AND
    A_grp_name = rand_nameA     AND
    A_modUser  = scs_user.system );

  -- explicit grp_id
  A := scs.new_obj_id;
  id := scs_group.new( 
    grp_id => A,
    grp_name => 'A', 
    email => 'A',  
    modifying_user => scs_user.system );
  scs_test.testBool( 'new', 2, id = A );

  -- explicit grp_type_id
  grp_type_id1 := scs_grp_type.new( 
    grp_type		=> scs_random.rand_string(10),
    modifying_user	=> scs_user.system );
  B := scs_group.new( 
    grp_type_id	      => grp_type_id1,
    grp_name	      => 'B', 
    email	      => 'B', 
    modifying_user    => scs_user.system );
  select grp_type_id into B_grp_type_id 
    from scs_groups
   where grp_id = B;
  scs_test.testBool( 'new', 3, B_grp_type_id = grp_type_id1 );

  -- explicit email
  C := scs_group.new( 
    grp_name	      => 'C', 
    email	      => 'C',
    modifying_user    => scs_user.system );
  select email into C_email
    from scs_parties
   where party_id = C;
  scs_test.testBool( 'new', 4, C_email = 'c' );

  -- explicit URL
  D := scs_group.new( 
    grp_name	     => 'D', 
    email	     => 'D', 
    url		     => 'www.itu.dk/',		      
    modifying_user   => scs_user.system );
  select url into D_url
    from scs_parties
   where party_id = D;
  scs_test.testBool( 'new', 5, D_url = 'www.itu.dk/' );

  -- explicit join policy: 'open'
  E := scs_group.new( 
    grp_name => 'E', 
    email => 'E', 
    join_policy => 'open',		      
    modifying_user => scs_user.system );
  select join_policy into E_join
    from scs_groups
   where grp_id = E;
  scs_test.testBool( 'new', 6, E_join = 'open' );

  -- explicit join policy: 'needs approval'
  F := scs_group.new( 
    grp_name => 'F', 
    email => 'F', 
    join_policy => 'needs approval',		      
    modifying_user => scs_user.system );
  select join_policy into F_join
    from scs_groups
   where grp_id = F;
  scs_test.testBool( 'new', 7, F_join = 'needs approval' );

  -- explicit join policy: 'closed'  
  G := scs_group.new( 
    grp_name => 'G', 
    email => 'G', 
    join_policy => 'closed',		      
    modifying_user => scs_user.system );
  select join_policy into G_join
    from scs_groups
   where grp_id = G;
  scs_test.testBool( 'new', 8, G_join = 'closed' );

  -- illegal values: there exists a group in scs_groups with this grp_id
  scs_test.testExn( 'new', 9, '
    declare
      id	integer;
    begin
      id := scs_group.new( 
        grp_id		    => ' || A || ',
        grp_name	    => scs_random.rand_string(10),
	modifying_user	    => scs_user.system
      );
    end;', 'f' );

  -- illegal values: there exists a party in scs_parties with party_id 'grp_id'
  rand_name1 := scs_random.rand_string(10);
  party_id1 := scs_party.new ( email => rand_name1 );
  scs_test.testExn( 'new', 10, '
    declare
      id	integer;
    begin
      id := scs_group.new( 
        grp_id		    => ' || party_id1 || ',
        grp_name	    => scs_random.rand_string(10),
	modifying_user	    => scs_user.system
      );
    end;', 'f' );

  -- illegal values: no group type exists in scs_grp_types with grp_type_id
  scs_test.testExn( 'new', 11, '
    declare
      id	integer;
    begin
      id := scs_group.new( 
        grp_name	    => scs_random.rand_string(10),
	grp_type_id	    => ' || illegal_id || ',
	modifying_user	    => scs_user.system
      );
    end;', 'f' );

  -- illegal values: there exists a party in scs_parties with this email  
  scs_test.testExn( 'new', 12, '
    declare
      id	integer;
    begin
      id := scs_group.new( 
        email		    => ''' || rand_name1 || ''',
        grp_name	    => scs_random.rand_string(10),
	modifying_user	    => scs_user.system
      );
    end;', 'f' );

  -- illegal values: there exists a group with this grp_name
  scs_test.testExn( 'new', 13, '
    declare
      id	integer;
    begin
      id := scs_group.new( 
        grp_name	    => ''F'',
	modifying_user	    => scs_user.system
      );
    end;', 'f' );

  -- illegal values: join policy is not 'open', 'needs approval' or 'closed'
  scs_test.testExn( 'new', 14, '
    declare
      id	integer;
    begin
      id := scs_group.new( 
        grp_name	    => scs_random.rand_string(10),
	join_policy	    => scs_random.rand_string(10),
	modifying_user	    => scs_user.system
      );
    end;', 'f' );

  -- illegal values: no user exists with user_id 'modifying_user'
  scs_test.testExn( 'new', 15, '
    declare
      id	integer;
    begin
      id := scs_group.new( 
        grp_name	    => scs_random.rand_string(10),
	modifying_user	    => ' || illegal_id || '
      );
    end;', 'f' );

  -- testing getID
  scs_test.printl( 'testing function ''getID'':');
  scs_test.testBool( 'getId', 1, A = scs_group.getID('A') );
  scs_test.testBool( 'getId', 2, scs_group.getID(illegal_id) is null );

  -- testing name
  scs_test.printl( 'testing function ''name'':');
  scs_test.testBool( 'name', 1, 'A' = scs_group.name(A) );
  scs_test.testBool( 'name', 2, scs_group.name( illegal_id) is null );

  -- Create the test members.
  scs_test.print('[Creating test users...');
  joe   := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p1 Joe', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jane  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p2 Jane', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  bob   := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p3 Bob', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  betty := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p4 Betty', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jack  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Jack', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jill  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Jill', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  sven  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Sven', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  stacy := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Stacy', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  scs_test.printl(' done]');

--  delete from scs_logs;

  scs_test.print('[Build member and composition relations... ');
  --          B  -> A  
  --        /  \  /
  --       D     C   <-  E
  --     /  \   / \      |
  --   joe  jane  bob  betty
  select count(*) into counter1_b from scs_grp_party_index;
  A_rel_id_B := scs_grp_composition_rel.new(
    grp_id_one => A, 
    grp_id_two => B, 
    modifying_user => scs_user.system );
  B_rel_id_C := scs_grp_composition_rel.new( 
    grp_id_one => B, 
    grp_id_two => C, 
    modifying_user => scs_user.system );
  A_rel_id_C := scs_grp_composition_rel.new( 
    grp_id_one => A, 
    grp_id_two => C, 
    modifying_user => scs_user.system );
  B_rel_id_D := scs_grp_composition_rel.new( 
    grp_id_one => B, 
    grp_id_two => D, 
    modifying_user => scs_user.system);
  C_rel_id_E := scs_grp_composition_rel.new( 
    grp_id_one => C, 
    grp_id_two => E, 
    modifying_user => scs_user.system );
  -- adding members
  C_rel_id_bob := scs_grp_member_rel.new(
    grp_id => C, party_id => bob, modifying_user => scs_user.system);
  D_rel_id_jane := scs_grp_member_rel.new( 
    grp_id => D, party_id => jane, modifying_user => scs_user.system);
  C_rel_id_jane := scs_grp_member_rel.new(
    grp_id => C, party_id => jane, modifying_user => scs_user.system);
  D_rel_id_joe := scs_grp_member_rel.new(
    grp_id => D, party_id => joe, modifying_user => scs_user.system);
  E_rel_id_betty := scs_grp_member_rel.new(
    grp_id => E, party_id => betty, modifying_user => scs_user.system);
  -- now the scs_party_index should look like this:

  -- grp_id     party_id     rel_id	      container_id		ans_rel_type
  -- A		B	     A_rel_id_B	      A				C
  -- B		C	     B_rel_idC	      B				C
  -- A		C	     B_rel_idC	      B				C
  -- A		C	     A_rel_id_C	      A				C
  -- B		D	     B_rel_id_D	      B				C
  -- A		D	     B_rel_id_D	      B				C
  -- C		E	     C_rel_id_E	      C				C
  -- B		E	     C_rel_id_E	      C				C
  -- A		E	     C_rel_id_E	      C				C
  -- C		bob	     C_rel_id_bob     C				M
  -- B		bob	     C_rel_id_bob     C				M
  -- A		bob	     C_rel_id_bob     C				M
  -- D		jane	     D_rel_id_jane    D				M
  -- B		jane	     D_rel_id_jane    D				M
  -- A		jane	     D_rel_id_jane    D				M
  -- C		jane	     C_rel_id_jane    C				M
  -- B		jane	     C_rel_id_jane    C				M
  -- A		jane	     C_rel_id_jane    C				M
  -- D		joe	     D_rel_id_joe     D				M
  -- B		joe	     D_rel_id_joe     D				M
  -- A		joe	     D_rel_id_joe     D				M
  -- E		betty	     E_rel_id_betty   E				M
  -- C		betty	     C_rel_id_betty   E				M
  -- B		betty	     C_rel_id_betty   E				M
  -- A		betty	     C_rel_id_betty   E				M
  scs_test.printl(' done]');

  -- Test size of scs_grp_party_index - we expects 25 rows.
  select count(*) into counter1_a from scs_grp_party_index;
  scs_test.testBool('Size of grp_party_index', 1, counter1_a = counter1_b + 25);

  scs_test.printl( 'testing function ''member_p'':' );
  --  DM = Direct Member of
  --   M = Member of
  scs_test.testBool( 'member_p', 1, 
  -- joe DM D(true) 
  scs_group.member_p(party_id => joe, grp_id => D, cascade_membership=>'f') = 't' AND 
  -- joe DM B (false)  
  scs_group.member_p(party_id => joe, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- joe  M B(true)  
  scs_group.member_p(party_id => joe, grp_id => B, cascade_membership=>'t') = 't' AND
  -- joe DM A (false)  
  scs_group.member_p(party_id => joe, grp_id => A, cascade_membership=>'f') = 'f' AND
  -- joe  M A(true)  
  scs_group.member_p(party_id => joe, grp_id => A, cascade_membership=>'t') = 't' AND
  -- joe  M C (false)
  scs_group.member_p(party_id => joe, grp_id => C, cascade_membership=>'t') = 'f'
  );

  scs_test.testBool( 'member_p', 2, 
  -- jane  M C(true)   
  scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t') = 't' AND
  -- jane DM C(true)   
  scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f') = 't' AND
  -- jane  M D(true)   
  scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t') = 't' AND
  -- jane DM D(true)   
  scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f') = 't' AND
  -- jane  M B(true)   
  scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t') = 't' AND
  -- jane DM B (false)   
  scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- jane  M A(true)   
  scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t') = 't' AND
  -- jane DM A (false)   
  scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f') = 'f'
  );

  scs_test.testBool( 'member_p', 3, 
  -- bob DM C(true)  
  scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  -- bob  M C(true)  
  scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  -- bob DM A (false)  
  scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  -- bob  M A(true)  
  scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 't' AND
  -- bob DM B (false)  
  scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- bob  M B(true)  
  scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 't'
  );

  scs_test.testBool( 'member_p', 4, 
  -- betty DM E(true)  
  scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f') = 't' AND
  -- betty  M E(true)  
  scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t') = 't' AND
  -- betty DM C (false)  
  scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f') = 'f' AND
  -- betty  M C(true)  
  scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t') = 't' AND
  -- betty DM B (false)  
  scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- betty  M B(true)  
  scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t') = 't' AND
  -- betty DM A (false)  
  scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f') = 'f' AND
  -- betty  M A(true)  
  scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t') = 't');

  -- illegal values: party_id
  scs_test.testUnit( 'member_p', 5, '
    declare
      flag char(1);
    begin
      flag := scs_group.member_p ( party_id           => ' || illegal_id || ',	 
				   grp_id	      => ' || A ||', 
				   cascade_membership => ''t'' );
    end;' );

  -- illegal values: party_id is null
  scs_test.testUnit( 'member_p', 6, '
    declare
      flag char(1);
    begin
      flag := scs_group.member_p ( party_id           => null,
				   grp_id	      => ' || A ||', 
				   cascade_membership => ''t'' );
    end;' );

  -- illegal values: grp_id
  scs_test.testUnit( 'member_p', 7, '
    declare
      flag char(1);
    begin
      flag := scs_group.member_p ( party_id           => ' || betty || ',	 
				   grp_id	      => ' || illegal_id ||', 
				   cascade_membership => ''t'' );
    end;' );

  -- illegal values: grp_id is null
  scs_test.testUnit( 'member_p', 8, '
    declare
      flag char(1);
    begin
      flag := scs_group.member_p ( party_id           => ' || betty || ',
				   grp_id	      => null,
				   cascade_membership => ''t'' );
    end;' );

  -- illegal values: cascade_mamberships
  scs_test.testUnit( 'member_p', 9, '
    declare
      flag char(1);
    begin
      flag := scs_group.member_p ( party_id           => ' || betty || ',	 
				   grp_id	      => ' || A ||', 
				   cascade_membership => ''g'' );
    end;' );

  -- illegal values: cascade_mamberships is null
  scs_test.testUnit( 'member_p', 10, '
    declare
      flag char(1);
    begin
      flag := scs_group.member_p ( party_id           => ' || betty || ',
				   grp_id	      => ' || A || ',
				   cascade_membership => null );
    end;' );
  
  -- Remove the test groups
  scs_group.destroy(G);
  scs_group.destroy(F);
  scs_group.destroy(E);
  scs_group.destroy(D);
  scs_group.destroy(C);
  scs_group.destroy(B);
  scs_group.destroy(A);

  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );
    delete scs_groups where grp_id in 
      ( A, B, C, D, E, F, G, grp_no_email_id);
    delete scs_grp_types where grp_type_id in ( grp_type_id1 );
    scs_test.print( '[Deleting test users...' );
    delete scs_user_preferences where user_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_users where user_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_persons where person_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_parties where party_id in 
      ( A, B, C, D, E, F, G, grp_no_email_id, party_id1,
        joe, jane, bob, betty, jack, jill, sven, stacy );
    scs_test.printl( ' done]' );

    select count(*) into counter0_a from scs_groups;
    select count(*) into counter00_a from scs_grp_types;
    select count(*) into counter000_a from scs_parties;
    select count(*) into counter0000_a from scs_users;
  
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
    scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
    scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
    scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );

  when others then
    scs_test.print( 'an error occured: ' );
    scs_test.printl( SQLERRM );
    goto clean;
end; -- of test of scs_group package
/
show errors

-- testing scs_grp_composition_rel package
declare
  A			integer;
  B			integer;
  C			integer;
  D			integer;
  E			integer;
  F			integer;
  G			integer;
  grp_no_email_id	integer;

  illegal_id		integer;
  grp_type_id1		integer;
  A_grp_name		varchar2(100);
  A_modUser		integer;
  B_grp_type_id		integer;
  C_email		varchar2(100);
  D_url			varchar2(200);
  E_join		varchar2(20);
  F_join		varchar2(20);
  G_join		varchar2(20);

  joe			integer;
  jane			integer;
  bob			integer;
  betty			integer;
  jack			integer;
  jill			integer;
  sven			integer;
  stacy			integer;

  A_rel_id_B		integer;
  B_rel_id_C		integer;
  B_rel_id_C2		integer;
  B_rel_id_D		integer;
  A_rel_id_C		integer;
  C_rel_id_E		integer;
  C_rel_id_bob		integer;
  C_rel_id_jane		integer;
  D_rel_id_jane		integer;
  D_rel_id_joe		integer;
  B_rel_id_C_member	integer;
  E_rel_id_betty	integer;

  party_id1		integer;

  counter1_b		integer;
  counter1_a		integer;
  counter2_b		integer;
  counter2_a		integer;
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
  clean_up		exception;
begin
  scs_test.printl( '---------------------------------------' );
  scs_test.printl( 'testing scs_grp_composition_rel package' );
  scs_test.printl( '---------------------------------------' );

  select count(*) into counter0_b from scs_groups;
  select count(*) into counter00_b from scs_grp_composition_rels;
  select count(*) into counter000_b from scs_parties;
  select count(*) into counter0000_b from scs_users;
  select count(*) into counter00000_b from scs_grp_party_index;

  illegal_id := scs.new_obj_id;

  -- creating groups A-G
  A := scs_group.new( 
    grp_name => 'A', 
    email => 'A',  
    modifying_user => scs_user.system );
  B := scs_group.new( 
    grp_name	      => 'B', 
    email	      => 'B', 
    modifying_user    => scs_user.system );
  C := scs_group.new( 
    grp_name	      => 'C', 
    email	      => 'C',
    modifying_user    => scs_user.system );
  D := scs_group.new( 
    grp_name	     => 'D', 
    email	     => 'D', 
    url		     => 'www.itu.dk/',		      
    modifying_user   => scs_user.system );
  E := scs_group.new( 
    grp_name => 'E', 
    email => 'E', 
    modifying_user => scs_user.system );
  F := scs_group.new( 
    grp_name => 'F', 
    email => 'F', 
    modifying_user => scs_user.system );
  G := scs_group.new( 
    grp_name => 'G', 
    email => 'G', 
    modifying_user => scs_user.system );

  -- Create the test users
  joe   := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p1 Joe', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jane  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p2 Jane', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  bob   := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p3 Bob', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  betty := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p4 Betty', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jack  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Jack', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jill  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Jill', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  sven  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Sven', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  stacy := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Stacy', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);


  select count(*) into counter1_b from scs_grp_party_index;

  A_rel_id_C := scs_grp_composition_rel.new( 
    grp_id_one => A, 
    grp_id_two => C, 
    modifying_user => scs_user.system );
  B_rel_id_D := scs_grp_composition_rel.new( 
    grp_id_one => B, 
    grp_id_two => D, 
    modifying_user => scs_user.system);
  C_rel_id_E := scs_grp_composition_rel.new( 
    grp_id_one => C, 
    grp_id_two => E, 
    modifying_user => scs_user.system );

  -- adding members
  C_rel_id_bob := scs_grp_member_rel.new(
    grp_id => C, party_id => bob, modifying_user => scs_user.system);
  D_rel_id_jane := scs_grp_member_rel.new( 
    grp_id => D, party_id => jane, modifying_user => scs_user.system);
  C_rel_id_jane := scs_grp_member_rel.new(
    grp_id => C, party_id => jane, modifying_user => scs_user.system);
  D_rel_id_joe := scs_grp_member_rel.new(
    grp_id => D, party_id => joe, modifying_user => scs_user.system);
  E_rel_id_betty := scs_grp_member_rel.new(
    grp_id => E, party_id => betty, modifying_user => scs_user.system);

  scs_test.printl( 'testing function ''new'':' );
  select count (*) into counter2_b from scs_grp_composition_rels;  
  A_rel_id_B := scs_grp_composition_rel.new(
    grp_id_one => A, 
    grp_id_two => B, 
    modifying_user => scs_user.system );  
  select count (*) into counter2_a from scs_grp_composition_rels;  
  scs_test.testBool( 'new', 1, counter2_a = counter2_b + 1 );

  B_rel_id_C := scs.new_obj_id;
  scs_test.testBool( 'new', 2, 
    B_rel_id_C = scs_grp_composition_rel.new( 
		   rel_id         => B_rel_id_C,
		   grp_id_one	  => B, 
		   grp_id_two	  => C, 
		   modifying_user => scs_user.system ) );

  -- illegal value: rel_id exists in the table scs_grp_composition_rels

  scs_test.testExn( 'new', 3, '
    declare
      id	integer;
    begin
      id := scs_grp_composition_rel.new( 
		   rel_id         => ' || B_rel_id_C || ',
		   grp_id_one	  => ' || B || ', 
		   grp_id_two	  => ' || C || ', 
		   modifying_user => scs_user.system );
    end;', 'f' );

  -- illegal value: grp_id_one is unknown
  scs_test.testExn( 'new', 4, '
    declare
      id	integer;
    begin
      id := scs_grp_composition_rel.new( 
		   grp_id_one	  => ' || illegal_id || ', 
		   grp_id_two	  => ' || C || ', 
		   modifying_user => scs_user.system );
    end;', 'f' );

  -- illegal value: grp_id_one is null
  scs_test.testExn( 'new', 4, '
    declare
      id	integer;
    begin
      id := scs_grp_composition_rel.new( 
		   grp_id_one	  => null,
		   grp_id_two	  => ' || C || ', 
		   modifying_user => scs_user.system );
    end;', 'f' );

  -- illegal value: grp_id_two is unknown
  scs_test.testExn( 'new', 4, '
    declare
      id	integer;
    begin
      id := scs_grp_composition_rel.new( 
		   grp_id_one	  => ' || B || ', 
		   grp_id_two	  => ' || illegal_id || ', 
		   modifying_user => scs_user.system );
    end;', 'f' );

  -- illegal value: grp_id_two is null
  scs_test.testExn( 'new', 4, '
    declare
      id	integer;
    begin
      id := scs_grp_composition_rel.new( 
		   grp_id_one	  => ' || B || ', 
		   grp_id_two	  => null,
		   modifying_user => scs_user.system );
    end;', 'f' );

  -- illegal value: modifying user is unknown
  scs_test.testExn( 'new', 4, '
    declare
      id	integer;
    begin
      id := scs_grp_composition_rel.new( 
		   grp_id_one	  => ' || B || ', 
		   grp_id_two	  => ' || C || ', 
		   modifying_user => ' || illegal_id || ' );
    end;', 'f' );

  -- illegal value: there is already a row with the pair (grp_id_one, grp_id_two)
  scs_test.testExn( 'new', 3, '
    declare
      id	integer;
    begin
      id := scs_grp_composition_rel.new( 
		   grp_id_one	  => ' || B || ', 
		   grp_id_two	  => ' || C || ', 
		   modifying_user => scs_user.system );
    end;', 'f' );


  scs_test.printl( 'testing function ''check_path_exists_p'':' );
  -- we know that D -> B -> A  
  scs_test.testBool( 'check_path_exists_p', 1, 
    scs_grp_composition_rel.check_path_exists_p( 
      component_id => D, container_id => B ) = 't' );
  scs_test.testBool( 'check_path_exists_p', 2, 
    scs_grp_composition_rel.check_path_exists_p( 
      component_id => D, container_id => A ) = 't' );
  scs_test.testBool( 'check_path_exists_p', 3, 
    scs_grp_composition_rel.check_path_exists_p( 
      component_id => A, container_id => A ) = 't' );
  scs_test.testBool( 'check_path_exists_p', 4, 
    scs_grp_composition_rel.check_path_exists_p( 
      component_id => D, container_id => C ) = 'f' );


  -- illegal values: component_id unknown
  scs_test.testUnit( 'check_path_exists_p', 5, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.check_path_exists_p( 
	        component_id => ' || illegal_id || ', 
		container_id => ' || C || ');
    end;' );

  -- illegal values: component_id is null
  scs_test.testUnit( 'check_path_exists_p', 6, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.check_path_exists_p( 
	        component_id => null,
		container_id => ' || C || ');
    end;' );

  -- illegal values: container_id unknown
  scs_test.testUnit( 'check_path_exists_p', 7, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.check_path_exists_p( 
	        component_id => ' || B || ', 
		container_id => ' || illegal_id || ');
    end;' );

  -- illegal values: container_id is null
  scs_test.testUnit( 'check_path_exists_p', 8, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.check_path_exists_p( 
	        component_id => ' || B || ', 
		container_id => null );
    end;' );

  scs_test.printl( 'testing function ''check_representation'':' );
  -- illegal values: rel_id unknown
  scs_test.testUnit( 'check_representation', 1, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.check_representation( 
		rel_id => ' || illegal_id || ');
    end;' );

  -- illegal values: rel_id is null
  scs_test.testUnit( 'check_representation', 2, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.check_representation( 
		rel_id => null );
    end;' );

  scs_test.printl( 'testing function ''contains_p'':' );
  scs_test.testBool( 'contains_p', 1, 
    scs_grp_composition_rel.contains_p( 
      grp_id	   => B,
      component_id => D ) = 't' );
  scs_test.testBool( 'contains_p', 2, 
    scs_grp_composition_rel.contains_p( 
      grp_id	   => A,
      component_id => D ) = 't' );
  scs_test.testBool( 'contains_p', 3, 
    scs_grp_composition_rel.contains_p( 
      grp_id	   => C,
      component_id => D ) = 'f' );
  scs_test.testBool( 'contains_p', 4, 
    scs_grp_composition_rel.contains_p( 
      grp_id	   => B,
      component_id => D,
      rel_id	   => B_rel_id_D ) = 't' );
  scs_test.testBool( 'contains_p', 5, 
    scs_grp_composition_rel.contains_p( 
      grp_id	   => B,
      component_id => D,
      rel_id	   => A_rel_id_B ) = 'f' );

  -- illegal values: grp_id is unknown
  scs_test.testUnit( 'contains_p', 6, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.contains_p( 
	        grp_id   => ' || illegal_id || ',
		component_id => ' || D || ');
    end;' );

  -- illegal values: grp_id is null
  scs_test.testUnit( 'contains_p', 7, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.contains_p( 
	        grp_id   => null,
		component_id => ' || D || ');
    end;' );

  -- illegal values: component_id is unknown
  scs_test.testUnit( 'contains_p', 8, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.contains_p( 
	        grp_id   => ' || D || ',
		component_id => ' || illegal_id || ' );
    end;' );

  -- illegal values: component_id is null
  scs_test.testUnit( 'contains_p', 9, '
    declare
      flag char(1);
    begin
      flag := scs_grp_composition_rel.contains_p( 
	        grp_id   => ' || D || ',
		component_id => null );
    end;' );


  -- testing 'check_representation_all' and 'destroy'
  scs_test.printl( 'testing function ''check_representation_all''
    and procedure ''destroy'':' );

  
  -- Remove B<--C composition and check membership for bob and betty
  select count(*) into counter2_b from scs_grp_composition_rels;
  scs_grp_composition_rel.destroy(B_rel_id_C, scs_user.system);
  select count(*) into counter2_a from scs_grp_composition_rels;
  scs_test.testBool( 'destroy', 1, counter2_a = counter2_b - 1 );
  -- membership and composition should look like this now:
  --          B  -> A  
  --        /      /
  --       D     C   <-  E
  --     /  \   / \      |
  --   joe  jane  bob  betty

  scs_test.testBool( 'check_representation_all', 1, 
    scs_group.check_representation_all = 't');

  scs_test.testBool( 'destroy', 2, 
  -- bob DM C(true)  
  scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  -- bob  M C(true)  
  scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  -- bob DM A (false)  
  scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  -- bob  M A(true)  
  scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 't' AND
  -- bob DM B (false)  
  scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- bob  M B (false)  
  scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 'f'
  );

  scs_test.testBool( 'destroy', 3, 
  -- betty DM E(true)  
  scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'f') = 't' AND
  -- betty  M E(true)  
  scs_group.member_p(party_id => betty, grp_id => E, cascade_membership=>'t') = 't' AND
  -- betty DM C (false)  
  scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'f') = 'f' AND
  -- betty  M C(true)  
  scs_group.member_p(party_id => betty, grp_id => C, cascade_membership=>'t') = 't' AND
  -- betty DM B (false)  
  scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- betty  M B (false)  
  scs_group.member_p(party_id => betty, grp_id => B, cascade_membership=>'t') = 'f' AND
  -- betty DM A (false)  
  scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'f') = 'f' AND
  -- betty  M A(true)  
  scs_group.member_p(party_id => betty, grp_id => A, cascade_membership=>'t') = 't'
  );

  -- Remove A<--C composition and check membership for bob and jane
  scs_grp_composition_rel.destroy(A_rel_id_C, scs_user.system);
  -- membership and composition should look like this now:
  --          B  -> A  
  --        /     
  --       D     C   <-  E
  --     /  \   / \      |
  --   joe  jane  bob  betty
  scs_test.testBool( 'check_representation_all', 2, 
    scs_group.check_representation_all = 't') ;

  scs_test.testBool( 'destroy', 4, 
  -- bob DM C(true)  
  scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'f') = 't' AND
  -- bob  M C(true)  
  scs_group.member_p(party_id => bob, grp_id => C, cascade_membership=>'t') = 't' AND
  -- bob DM A (false)  
  scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'f') = 'f' AND
  -- bob  M A (false)  
  scs_group.member_p(party_id => bob, grp_id => A, cascade_membership=>'t') = 'f' AND
  -- bob DM B (false)  
  scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- bob  M B (false)  
  scs_group.member_p(party_id => bob, grp_id => B, cascade_membership=>'t') = 'f'
  );

  scs_test.testBool( 'destroy', 5, 
  -- jane  M C(true)  
  scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'t') = 't' AND
  -- jane DM C(true)  
  scs_group.member_p(party_id => jane, grp_id => C, cascade_membership=>'f') = 't' AND
  -- jane  M D(true)  
  scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'t') = 't' AND
  -- jane DM D(true)  
  scs_group.member_p(party_id => jane, grp_id => D, cascade_membership=>'f') = 't' AND
  -- jane  M B(true)  
  scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'t') = 't' AND
  -- jane DM B (false)  
  scs_group.member_p(party_id => jane, grp_id => B, cascade_membership=>'f') = 'f' AND
  -- jane  M A(true)  
  scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'t') = 't' AND
  -- jane DM A (false)  
  scs_group.member_p(party_id => jane, grp_id => A, cascade_membership=>'f') = 'f');

  select count(*) into counter1_a from scs_grp_party_index;
  scs_test.testBool( 'Size of grp_party_index', 2, 
    counter1_a = counter1_b + 4+3+2+4+1 ); --should have 14 entries in the index

  select count(*) into counter2_b from scs_grp_composition_rels;
  -- illegal values: unknown rel_id
  scs_test.testUnit( 'destroy', 6, '
    begin
      scs_grp_composition_rel.destroy( 
        rel_id         => ' || illegal_id || ',
	modifying_user => scs_user.system );
    end;' );

  -- illegal values: rel_id is null 
  scs_test.testUnit( 'destroy', 7, '
    begin
      scs_grp_composition_rel.destroy( 
        rel_id         => null,
	modifying_user => scs_user.system );
    end;' );
  select count(*) into counter2_a from scs_grp_composition_rels;
  scs_test.testBool( 'destroy', 10, counter2_a = counter2_b );
  

  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );
    delete scs_grp_member_rels where rel_id in (
      C_rel_id_bob, C_rel_id_jane, D_rel_id_jane,
      D_rel_id_joe, B_rel_id_C_member, E_rel_id_betty );
    delete scs_grp_composition_rels where rel_id in (
      A_rel_id_B, B_rel_id_C, B_rel_id_C2, B_rel_id_D,
      A_rel_id_C, C_rel_id_E );
    delete scs_groups where grp_id in (
      A, B, C, D, E, F, G, grp_no_email_id);
    delete scs_grp_types where grp_type_id in ( grp_type_id1 );
    scs_test.print( '[Deleting test users...' );
    delete scs_user_preferences where user_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_users where user_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_persons where person_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_parties where party_id in 
      ( A, B, C, D, E, F, G, grp_no_email_id, party_id1,
        joe, jane, bob, betty, jack, jill, sven, stacy );
    scs_test.printl( ' done]' );

    select count(*) into counter0_a from scs_groups;
    select count(*) into counter00_a from scs_grp_composition_rels;
    select count(*) into counter000_a from scs_parties;
    select count(*) into counter0000_a from scs_users;
    select count(*) into counter00000_a from scs_grp_party_index;

    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
    scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
    scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
    scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );
    scs_test.testBool( 'garbage check', 5, counter00000_b = counter00000_a );

  when others then
    scs_test.print( 'an error occured: ' );
    scs_test.printl( SQLERRM );
    goto clean;
end; -- of testing scs_grp_composition_rel package
/
show errors


-- testing scs_grp_member_rel package
declare
  grp_id1		integer;
  A			integer;
  B			integer;
  C			integer;
  D			integer;
  E			integer;
  F			integer;
  G			integer;
  grp_no_email_id	integer;

  illegal_id		integer;

  grp_type_id1		integer;
  A_grp_name		varchar2(100);
  A_modUser		integer;
  B_grp_type_id		integer;
  C_email		varchar2(100);
  D_url			varchar2(200);
  E_join		varchar2(20);
  F_join		varchar2(20);
  G_join		varchar2(20);

  joe			integer;
  jane			integer;
  bob			integer;
  betty			integer;
  jack			integer;
  jill			integer;
  sven			integer;
  stacy			integer;


  rel_id1		integer;
  A_rel_id_B		integer;
  B_rel_id_C		integer;
  B_rel_id_C2		integer;
  B_rel_id_D		integer;
  A_rel_id_C		integer;
  C_rel_id_E		integer;
  D_rel_id_C		integer;
  C_rel_id_bob		integer;
  C_rel_id_jane		integer;
  D_rel_id_jane		integer;
  D_rel_id_joe		integer;
  B_rel_id_C_member	integer;
  E_rel_id_betty	integer;
  E_rel_id_jack		integer;
  A_rel_id_sven		integer;
  B_rel_id_stacy	integer;  

  member_state1		varchar(20);
  party_id1		integer;

  counter1_b		integer;
  counter1_a		integer;
  counter2_b		integer;
  counter2_a		integer;
  counter3_b		integer;
  counter3_a		integer;
  counter4_a		integer;
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

  clean_up		exception;
begin
  scs_test.printl( '----------------------------------' );
  scs_test.printl( 'testing scs_grp_member_rel package' );
  scs_test.printl( '----------------------------------' );

  select count(*) into counter0_b from scs_groups;
  select count(*) into counter00_b from scs_grp_member_rels;
  select count(*) into counter000_b from scs_parties;
  select count(*) into counter0000_b from scs_users;
  select count(*) into counter00000_b from scs_grp_party_index;

  illegal_id := scs.new_obj_id;

  -- creating groups A-G
  A := scs_group.new( 
    grp_name => 'A', 
    email => 'A',  
    modifying_user => scs_user.system );
  B := scs_group.new( 
    grp_name	      => 'B', 
    email	      => 'B', 
    modifying_user    => scs_user.system );
  C := scs_group.new( 
    grp_name	      => 'C', 
    email	      => 'C',
    modifying_user    => scs_user.system );
  D := scs_group.new( 
    grp_name	     => 'D', 
    email	     => 'D', 
    url		     => 'www.itu.dk/',		      
    modifying_user   => scs_user.system );
  E := scs_group.new( 
    grp_name => 'E', 
    email => 'E', 
    modifying_user => scs_user.system );
  F := scs_group.new( 
    grp_name => 'F', 
    email => 'F', 
    modifying_user => scs_user.system );
  G := scs_group.new( 
    grp_name => 'G', 
    email => 'G', 
    modifying_user => scs_user.system );

  -- Create the test users
  joe   := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p1 Joe', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jane  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p2 Jane', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  bob   := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p3 Bob', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  betty := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'p4 Betty', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jack  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Jack', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jill  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Jill', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  sven  := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Sven', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  stacy := scs_user.new(email => scs_random.rand_string(10),
	                first_names => 'Stacy', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);

  select count(*) into counter1_b from scs_grp_party_index;
  -- create group compositions
  A_rel_id_C := scs_grp_composition_rel.new( 
    grp_id_one => A, 
    grp_id_two => C, 
    modifying_user => scs_user.system );
  B_rel_id_D := scs_grp_composition_rel.new( 
    grp_id_one => B, 
    grp_id_two => D, 
    modifying_user => scs_user.system);
  C_rel_id_E := scs_grp_composition_rel.new( 
    grp_id_one => C, 
    grp_id_two => E, 
    modifying_user => scs_user.system );

  scs_test.printl( 'testing function ''new'':' );
  select count(*) into counter2_b from scs_grp_member_rels;
  C_rel_id_bob := scs_grp_member_rel.new(
    grp_id => C, party_id => bob, modifying_user => scs_user.system);
  select count(*) into counter2_a from scs_grp_member_rels;
  select grp_id, party_id, member_state into grp_id1, party_id1, member_state1 
    from scs_grp_member_rels
   where rel_id = C_rel_id_bob;
  scs_test.testBool( 'new', 1, counter2_a = counter2_b + 1 );
  scs_test.testBool( 'new', 2, 
    grp_id1 = C AND party_id1 = bob AND member_state1 = 'approved' );

  select count(*) into counter2_b from scs_grp_member_rels;
  D_rel_id_jane := scs.new_obj_id;
  rel_id1 := scs_grp_member_rel.new(
	       rel_id => D_rel_id_jane,
	       grp_id => D, 
	       party_id => jane, 
	       modifying_user => scs_user.system );
  scs_test.testBool( 'new', 3, rel_id1 = D_rel_id_jane );
  select count(*) into counter2_a from scs_grp_member_rels;
  select grp_id, party_id into grp_id1, party_id1 
    from scs_grp_member_rels
   where rel_id = D_rel_id_jane;
  scs_test.testBool( 'new', 4, counter2_a = counter2_b + 1 );
  scs_test.testBool( 'new', 5, grp_id1 = D AND party_id1 = jane );

  C_rel_id_jane := scs_grp_member_rel.new(
		     grp_id         => C, 
		     party_id	    => jane,
		     member_state   => 'needs approval',
		     modifying_user => scs_user.system );
  select grp_id, party_id, member_state into grp_id1, party_id1, member_state1 
    from scs_grp_member_rels
   where rel_id = C_rel_id_jane;
  scs_test.testBool( 'new', 6, 
    grp_id1 = C AND party_id1 = jane AND member_state1 = 'needs approval' );  

  D_rel_id_joe := scs_grp_member_rel.new(
		    grp_id => D, 
		    party_id => joe, 
		    member_state   => 'banned',
		    modifying_user => scs_user.system );
  select grp_id, party_id, member_state into grp_id1, party_id1, member_state1 
    from scs_grp_member_rels
   where rel_id = D_rel_id_joe;
  scs_test.testBool( 'new', 7, 
    grp_id1 = D AND party_id1 = joe AND member_state1 = 'banned' );  

  E_rel_id_betty := scs_grp_member_rel.new(
		      grp_id => E, 
		      party_id => betty, 
		      member_state   => 'rejected',
		      modifying_user => scs_user.system);
  select grp_id, party_id, member_state into grp_id1, party_id1, member_state1 
    from scs_grp_member_rels
   where rel_id = E_rel_id_betty;
  scs_test.testBool( 'new', 8, 
    grp_id1 = E AND party_id1 = betty AND member_state1 = 'rejected' );  

  E_rel_id_jack := scs_grp_member_rel.new(
		      grp_id => E, 
		      party_id => jack, 
		      member_state   => 'deleted',
		      modifying_user => scs_user.system);
  select grp_id, party_id, member_state into grp_id1, party_id1, member_state1 
    from scs_grp_member_rels
   where rel_id = E_rel_id_jack;
  scs_test.testBool( 'new', 9, 
    grp_id1 = E AND party_id1 = jack AND member_state1 = 'deleted' );  

  -- illegal values: existing rel_id
  scs_test.testExn( 'new', 10, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      rel_id         => ' || E_rel_id_jack || ',
	      grp_id         => ' || E ||', 
	      party_id	     => ' || jill || ',
	      modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: grp_id is unknown
  scs_test.testExn( 'new', 11, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => ' || illegal_id ||', 
	      party_id	     => ' || jill || ',
	      modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: grp_id is null
  scs_test.testExn( 'new', 12, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => null,
	      party_id	     => ' || jill || ',
	      modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: party_id is unknown
  scs_test.testExn( 'new', 13, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => ' || E ||', 
	      party_id	     => ' || illegal_id || ',
	      modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: party_id is null
  scs_test.testExn( 'new', 14, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => ' || E ||',
	      party_id	     => null,
	      modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: member_state is unknown
  scs_test.testExn( 'new', 15, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => ' || E ||', 
	      party_id	     => ' || jill || ',
	      member_state   => scs_random.rand_string(20),
	      modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: member_state is null
  scs_test.testExn( 'new', 16, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => ' || E ||', 
	      party_id	     => ' || jill || ',
	      member_state   => null,
	      modifying_user => scs_user.system);
    end;', 'f' );

  -- illegal values: modifying user is unknown
  scs_test.testExn( 'new', 17, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => ' || E ||', 
	      party_id	     => ' || jill || ',
	      modifying_user => ' || illegal_id || ');
    end;', 'f' );

  -- illegal values: modifying user is null
  scs_test.testExn( 'new', 18, '
    declare
      id	integer;
    begin
      id := scs_grp_member_rel.new(
	      grp_id         => ' || E ||', 
	      party_id	     => ' || jill || ',
	      modifying_user => null );
    end;', 'f' );

  scs_test.printl( 'testing procedure ''destroy'':' );  
  -- first rel_id version (1 parameter version)
  A_rel_id_sven := scs_grp_member_rel.new(
		     grp_id          => A,
		     party_id	     => sven,
		     modifying_user  => scs_user.system );
  B_rel_id_stacy := scs_grp_member_rel.new(
		     grp_id          => B,
		     party_id	     => stacy,
		     modifying_user  => scs_user.system );
  -- sven direct member of A
  select count(*) into counter2_b 
    from scs_grp_member_rels
   where rel_id = A_rel_id_sven;
  select count(*) into counter3_b from scs_grp_party_index;
  scs_grp_member_rel.destroy ( 
    rel_id         => A_rel_id_sven,
    modifying_user => scs_user.system );
  select count(*) into counter2_a
    from scs_grp_member_rels
   where rel_id = A_rel_id_sven;
  select count(*) into counter3_a from scs_grp_party_index;
  scs_test.testBool( 'destroy, rel_id version', 1, counter2_a = counter2_b - 1 );
  scs_test.testBool( 'destroy, rel_id version', 2, counter3_a = counter3_b - 1 );

  -- stacy direct member of B, A composed of B
  select count(*) into counter2_b 
    from scs_grp_member_rels
   where rel_id = B_rel_id_stacy;
  select count(*) into counter3_b from scs_grp_party_index;
  scs_grp_member_rel.destroy ( 
    rel_id         => B_rel_id_stacy,
    modifying_user => scs_user.system );
  select count(*) into counter2_a
    from scs_grp_member_rels
   where rel_id = B_rel_id_stacy;
  select count(*) into counter3_a from scs_grp_party_index;
  select count(*) into counter4_a 
    from scs_grp_party_index
   where rel_id = B_rel_id_stacy;
  scs_test.testBool( 'destroy, rel_id version', 3, counter2_a = counter2_b - 1 );
  scs_test.testBool( 'destroy, rel_id version', 4, counter3_a = counter3_b - 1 );
  scs_test.testBool( 'destroy, rel_id version', 5, counter4_a = 0 );

  -- try to create new composition and then destroy joes membership of D:
  D_rel_id_C := scs_grp_composition_rel.new( 
    grp_id_one => D, 
    grp_id_two => C, 
    modifying_user => scs_user.system );
  scs_grp_member_rel.destroy( rel_id => D_rel_id_joe );
  select count(*) into counter4_a 
    from scs_grp_party_index
   where rel_id = D_rel_id_joe;
  scs_test.testBool( 'destroy, rel_id version', 6, counter4_a = 0 );

  -- repeated new/destroy
  select count(*) into counter2_b from scs_grp_member_rels;
  select count(*) into counter3_b from scs_grp_party_index;
  scs_test.testUnit( 'destroy, rel_id version', 7, '
    declare
      id	integer;
      i		integer;
    begin
      for i in 1..1000 loop
        id := scs_grp_member_rel.new(
	        grp_id          => ' || A ||',
	        party_id	      => ' || sven ||',
	        modifying_user  => scs_user.system );
	scs_grp_member_rel.destroy( rel_id => id );
      end loop;
    end;' );
  select count(*) into counter2_a from scs_grp_member_rels;
  select count(*) into counter3_a from scs_grp_party_index;
  scs_test.testBool( 'destroy, rel_id version', 8, 
    counter2_a = counter2_b AND counter3_a = counter3_b );

  -- illegal values: rel_id is unknown
  scs_test.testUnit( 'destroy, rel_id version', 9, '
    begin
      scs_grp_member_rel.destroy( rel_id => ' || illegal_id || ' );
    end;' );

  -- illegal values: rel_id is null
  scs_test.testUnit( 'destroy, rel_id version', 10, '
    begin
      scs_grp_member_rel.destroy( rel_id => null );
    end;' );

  -- now (grp_id, party_id) version (2 parameter version)
  

  -- cleaning up
  scs_test.print( 'finished, ' );
  raise clean_up;

exception
  when clean_up then
    <<clean>>
    scs_test.printl( 'cleaning up:' );
    delete scs_grp_member_rels where rel_id in (
      C_rel_id_bob, C_rel_id_jane, D_rel_id_jane,
      D_rel_id_joe, B_rel_id_C_member, E_rel_id_betty,
      E_rel_id_jack, A_rel_id_sven ,B_rel_id_stacy );
    delete scs_grp_composition_rels where rel_id in (
      A_rel_id_B, B_rel_id_C, B_rel_id_C2, B_rel_id_D,
      A_rel_id_C, C_rel_id_E, D_rel_id_C );
    delete scs_groups where grp_id in (
      A, B, C, D, E, F, G, grp_no_email_id);
    delete scs_grp_types where grp_type_id in ( grp_type_id1 );
    scs_test.print( '[Deleting test users...' );
    delete scs_user_preferences where user_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_users where user_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_persons where person_id in (
        joe, jane, bob, betty, jack, jill, sven, stacy );
    delete scs_parties where party_id in 
      ( A, B, C, D, E, F, G, grp_no_email_id, party_id1,
        joe, jane, bob, betty, jack, jill, sven, stacy );
    scs_test.printl( ' done]' );

    select count(*) into counter0_a from scs_groups;
    select count(*) into counter00_a from scs_grp_member_rels;
    select count(*) into counter000_a from scs_parties;
    select count(*) into counter0000_a from scs_users;
    select count(*) into counter00000_a from scs_grp_party_index;
  
    scs_test.testBool( 'garbage check', 1, counter0_b = counter0_a );
    scs_test.testBool( 'garbage check', 2, counter00_b = counter00_a );
    scs_test.testBool( 'garbage check', 3, counter000_b = counter000_a );
    scs_test.testBool( 'garbage check', 4, counter0000_b = counter0000_a );
    scs_test.testBool( 'garbage check', 5, counter00000_b = counter00000_a );

  when others then
    scs_test.print( 'an error occured: ' );
    scs_test.printl( SQLERRM );
    goto clean;
end; -- of testing scs_grp_composition_rel package
/
show errors

-- If you have errors, then look in the scs-log table.
-- select message
--   from scs_logs;

drop procedure print_scs_grp_party_index;

