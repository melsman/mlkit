-- $Id$

-- This code i a down-sized and modified version of the acs-logs module
-- found in openACS (www.openacs.org): files acs-logs-create.sql

/* ======================================================================
   package scs_log

   logging functionality

   History:
   141102 Kennie Nybo Pontoppidan <kennie@it-c.dk> Added comments
   091002 Niels Hallenberg <nh@it.edu> Created package
====================================================================== */
create or replace package scs_log
as
  /* ----------------
     procedure notice
     ----------------
     inserts a 'notice' entry in the table scs_logs 
  */
  procedure notice (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

  /* ----------------
     procedure warn
     ----------------
     inserts a 'warn' entry in the table scs_logs 
  */
  procedure warn (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

  /* ----------------
     procedure error
     ----------------
     inserts an 'error' entry in the table scs_logs 
  */
  procedure error (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

  /* ----------------
     procedure debug
     ----------------
     inserts a 'debug' entry in the table scs_logs 
  */
  procedure debug (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

end;
/
show errors


/*======================================================================
  package bodies begin here
====================================================================== */
create or replace package body scs_log
as
  procedure notice (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  )
  is
  begin
    insert into scs_logs
     (log_id, log_level, log_key, message)
    values
     (scs_log_id_seq.nextval, 'notice', notice.log_key, notice.message);
  end;

  procedure warn (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  )
  is
  begin
    insert into scs_logs
     (log_id, log_level, log_key, message)
    values
     (scs_log_id_seq.nextval, 'warn', warn.log_key, warn.message);
  end;

  procedure error (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  )
  is
  begin
    insert into scs_logs
     (log_id, log_level, log_key, message)
    values
     (scs_log_id_seq.nextval, 'error', error.log_key, error.message);
  end;

  procedure debug (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  )
  is
  begin
    insert into scs_logs
     (log_id, log_level, log_key, message)
    values
     (scs_log_id_seq.nextval, 'debug', debug.log_key, debug.message);
  end;

end;
/
show errors
