-- $Id$

create table scs_priority_rels (
  rel_id integer
    constraint scs_prio_rels_rel_id_nn not null
    constraint scs_prio_rels_rel_id_pk primary key,
  on_what_parent_table varchar2(30)
    constraint scs_prio_rels_on_w_p_table_nn not null,
  on_which_parent_id integer
    constraint scs_prio_rels_on_w_p_id_nn not null,
  on_what_child_table varchar2(30)
    constraint scs_prio_rels_on_w_c_table_nn not null,
  on_which_child_id integer
    constraint scs_prio_rels_on_w_c_id_nn not null,
  priority integer default 1
    constraint scs_prio_rels_priority_nn not null,
  constraint scs_prio_rels_un 
    unique (on_what_parent_table, on_which_parent_id, 
	    on_what_child_table, priority),
  created_on date default sysdate
    constraint scs_prio_rels_created_on_nn not null,
  last_modified date default sysdate 
    constraint scs_prio_rels_last_modified_nn not null,
  modifying_user integer
);


