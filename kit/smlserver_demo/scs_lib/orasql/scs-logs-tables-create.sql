-- $Id$

-- This code i a down-sized and modified version of the acs-logs module
-- found in openACS (www.openacs.org): files acs-logs-create.sql

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

-- sequence used in the package scs_log
create sequence scs_log_id_seq start with 1000;
