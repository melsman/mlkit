-- $Id$

-----------------------------
-- INITIAL GROUP TYPE DATA --
-----------------------------
declare
  v_grp_type_id scs_grp_types.grp_type_id%TYPE;
begin
  v_grp_type_id := 
    scs_grp_type.new (grp_type => 'default',
                      default_grp_type => 't',
                      modifying_user => scs_user.system);
end;
/
show errors
