/* ======================================================================
   package scs_approvals

   functionality for approving rows

   $Id$

   History:
   040403 Kennie Nybo Pontoppidan <kennie@it-c.dk> 
   changed approve_row to take modified_by argument
   160303 Kennie Nybo Pontoppidan <kennie@it-c.dk> created package
   210303 Niels Hallenberg <nh@it-c.dk> modified package to reflect
          changes in scs_approvals table
====================================================================== */ 
create or replace package scs_approval
is
  /* ---------------------
     procedure approve_row
     ---------------------
     inserts a row in scs_approvals with 
       ON_WHAT_TABLE = table_name
       ON_WHICH_ID = id
       USER_ID = approved_by
       DESICION = 't'
       CREATED_ON = sysdate,
       LAST_MODIFIED = sysdate,
       MODIFYING_USER = modified_by
  */
  procedure approve_row(
    table_name  in varchar2,
    id          in integer,
    approved_by in integer,
    modified_by in integer
  );

  /* ---------------------
     procedure decline_row
     ---------------------
     inserts a row in scs_approvals with 
       ON_WHAT_TABLE = table_name
       ON_WHICH_ID = id
       USER_ID = approved_by
       DESICION = 'f'
       CREATED_ON = sysdate
       NOTE_TEXT
  */
  procedure decline_row(
    table_name  in varchar2,
    id          in integer,
    approved_by in integer,
    note_text   in varchar,
    modified_by in integer
  );
  
  /* ---------------------
     procedure delete_rows
     ---------------------
     delete all rows in scs_approvals with 
       ON_WHAT_TABLE = table_name
       ON_WHICH_ID = id
  */
  procedure delete_rows(
    table_name  in varchar2,
    id          in integer
  );


end scs_approval;
/ 
show errors

create or replace package body scs_approval
is
  procedure approve_row(
    table_name  in varchar2,
    id          in integer,
    approved_by in integer,
    modified_by in integer
  ) is
  begin
    insert into SCS_APPROVALS(
      APPROVAL_ID,   
      ON_WHAT_TABLE, 
      ON_WHICH_ID,   
      USER_ID,       
      DECISION,      
      CREATED_ON,    
      LAST_MODIFIED, 
      MODIFYING_USER
    ) values(
      scs.new_obj_id,
      table_name,
      id,
      approved_by,
      't',
      sysdate,
      sysdate,
      modified_by
    );
  end approve_row;

  procedure decline_row(
    table_name  in varchar2,
    id          in integer,
    approved_by in integer,
    note_text   in varchar,
    modified_by in integer
  ) is
  begin
    insert into SCS_APPROVALS(
      APPROVAL_ID,    
      ON_WHAT_TABLE,  
      ON_WHICH_ID,     
      USER_ID,      
      DECISION,       
      NOTE_TEXT,
      CREATED_ON,
      LAST_MODIFIED, 
      MODIFYING_USER
    ) values(
      scs.new_obj_id,
      table_name,
      id,
      approved_by,
      'f',
      note_text,
      sysdate,
      sysdate,
      modified_by
    );
  end decline_row;

  procedure delete_rows(
    table_name  in varchar2,
    id          in integer
  ) is
  begin
    delete scs_approvals
     where ON_WHAT_TABLE = table_name
       and ON_WHICH_ID = id;
  end delete_rows;

end scs_approval;
/ 
show errors

