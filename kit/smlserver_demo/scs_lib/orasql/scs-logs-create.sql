-- This code i a down-sized and modified version of the acs-logs module
-- found in openACS (www.openacs.org): files acs-logs-create.sql

create sequence scs_log_id_seq start with 1000;

create table scs_logs (
 log_id integer
   constraint scs_logs_pk primary key,
 log_date date default sysdate
   constraint scs_logs_log_date_nn not null,
 log_level varchar2(20)
   constraint scs_logs_log_level_nn not null
   constraint scs_logs_log_level_ck 
     check (log_level in ('notice', 'warn', 'error', 'debug')),
 log_key varchar2(100)
   constraint scs_logs_log_key_nn not null,
 message varchar2(4000)
   constraint scs_logs_message_nn not null
);

-------------------------
-- Package Description --
-------------------------
create or replace package scs_log
as
  procedure notice (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

  procedure warn (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

  procedure error (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

  procedure debug (
    log_key in scs_logs.log_key%TYPE,
    message in scs_logs.message%TYPE
  );

end;
/
show errors

------------------
-- Package Body --
------------------
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
