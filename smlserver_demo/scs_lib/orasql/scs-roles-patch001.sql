/* 
Patch 001 for scs-roles

Date:   2004-04-01
Author: Kennie Nybo Pontoppidan
Purpose: 
* Adding primary key rel_id on scs_role_rels
*/

alter table scs_role_rels
  add rel_id integer;

declare
begin
for rel_rec in (
  select * from scs_role_rels
)
loop
  if rel_rec.rel_id is null then
    update scs_role_rels
       set rel_id = scs.new_obj_id;
  end if;
end loop; 
end;
/
show errors

alter table scs_role_rels modify (
  rel_id
    constraint scs_role_rels_rel_id_nn not null
);

alter table scs_role_rels 
  add ( primary key(rel_id) );

@scs-roles-packages-create
