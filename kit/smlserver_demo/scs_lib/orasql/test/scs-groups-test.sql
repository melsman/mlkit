-- This code i a modified version of the acs-groups-test module found
-- in openACS (www.openacs.org): files acs-groups-test.sql

set serveroutput on

declare
  A      integer;
  B      integer;
  C      integer;
  D      integer;
  E      integer;
  F      integer;
  G      integer;

  joe    integer;
  jane   integer;
  bob    integer;
  betty  integer;
  jack	 integer;
  jill	 integer;
  sven	 integer;
  stacy	 integer;

  rel_id integer;
begin
  -- Create the test groups.
  A := scs_group.new(grp_name => 'A', modifying_user => scs_user.system);
  B := scs_group.new(grp_name => 'B', modifying_user => scs_user.system);
  C := scs_group.new(grp_name => 'C', modifying_user => scs_user.system);
  D := scs_group.new(grp_name => 'D', modifying_user => scs_user.system);
  E := scs_group.new(grp_name => 'E', modifying_user => scs_user.system);
  F := scs_group.new(grp_name => 'F', modifying_user => scs_user.system);
  G := scs_group.new(grp_name => 'G', modifying_user => scs_user.system);

  -- Create the test members.
  joe   := scs_user.new(email => 'joe@asdf.com',
	                first_names => 'Joe', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jane  := scs_user.new(email => 'jane@asdf.com',
	                first_names => 'Jane', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  bob   := scs_user.new(email => 'bob@asdf.com',
	                first_names => 'Bob', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  betty := scs_user.new(email => 'betty@asdf.com',
	                first_names => 'Betty', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jack  := scs_user.new(email => 'jack@asdf.com',
	                first_names => 'Jack', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  jill  := scs_user.new(email => 'jill@asdf.com',
	                first_names => 'Jill', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  sven  := scs_user.new(email => 'sven@asdf.com',
	                first_names => 'Sven', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);
  stacy := scs_user.new(email => 'stacy@asdf.com',
	                first_names => 'Stacy', last_name => 'Smith',
		        password => 'assword', salt => 'p', 
			modifying_user => scs_user.system);

  -- Make a couple of compositions.

--   rel_id := composition_rel.new(object_id_one => A, object_id_two => B);
--   rel_id := composition_rel.new(object_id_one => A, object_id_two => C);
--   rel_id := composition_rel.new(object_id_one => A, object_id_two => D);

--   rel_id := composition_rel.new(object_id_one => E, object_id_two => A);
--   rel_id := composition_rel.new(object_id_one => F, object_id_two => A);
--   rel_id := composition_rel.new(object_id_one => G, object_id_two => A);

  -- Make a couple of memberships.

--   rel_id := membership_rel.new(object_id_one => B, object_id_two => joe);
--   rel_id := membership_rel.new(object_id_one => B, object_id_two => jane);
--   rel_id := membership_rel.new(object_id_one => B, object_id_two => betty);
--   rel_id := membership_rel.new(object_id_one => A, object_id_two => bob);
--   rel_id := membership_rel.new(object_id_one => A, object_id_two => betty);
--   rel_id := membership_rel.new(object_id_one => E, object_id_two => betty);

  delete from scs_logs;

--   for g in (select * from groups) loop
--     if scs_group.check_representation(g.grp_id) = 'f' then
--       dbms_output.put_line('Group ' || g.grp_name || ' (' || g.grp_id ||
-- 			   ') failed.');
--     end if;
--   end loop;

  -- Remove the test groups.
  scs_group.delete(G);
  scs_group.delete(F);
  scs_group.delete(E);
  scs_group.delete(D);
  scs_group.delete(C);
  scs_group.delete(B);
  scs_group.delete(A);

  -- Remove the test members.
  scs_user.delete(joe);
  scs_user.delete(jane);
  scs_user.delete(bob);
  scs_user.delete(betty);
  scs_user.delete(jack);
  scs_user.delete(jill);
  scs_user.delete(sven);
  scs_user.delete(stacy);
end;
/
show errors


select log_level, log_key, message
from scs_logs
where log_key = 'error';
