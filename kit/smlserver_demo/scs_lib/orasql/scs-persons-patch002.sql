/* 
Patch 002 for scs-persons

Date:   2003-11-17
Author: Kennie Nybo Pontoppidan
Purpose: 
* Adding field full_name_lower
*/

alter table scs_persons 
  add full_name_lower varchar2(200);

update scs_persons
set full_name_lower = lower(replace(first_names, ' ', ''))  || lower(last_name);

@scs-persons-packages-create.sql

alter table scs_persons  modify (
  full_name_lower
    constraint scs_persons_fnl_nn not null
);

create or replace view scs_persons_active as
select * 
  from scs_persons
 where deleted_p = 'f';

create index scs_persons_idx on scs_persons( full_name_lower );
