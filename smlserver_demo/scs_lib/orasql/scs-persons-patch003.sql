/* 
Patch 003 for scs_persons

Date:   2003-11-19
Author: Kennie Nybo Pontoppidan
Purpose: 
* Adding index on scs_person_rels
*/

create index scs_person_rels_idx on scs_person_rels( on_what_table );
