-- $Id$

-- This code i a down-sized and modified version of the users module found
-- in openACS (www.openacs.org): file community-core-create.sql.


/* ========================================
   table scs_users

   describes a user

   NB (nh 2002-10-20): The field state below follows the 
		       Phil'g registration cyclus 
		       - yet to be implemented
======================================== */
create table scs_users (
  user_id integer
    constraint scs_users_user_id_nn not null
    constraint scs_users_user_id_fk references scs_persons(person_id)
    constraint scs_users_pk primary key,
  password varchar(40),
  salt varchar(40),
  screen_name varchar2(100)
    constraint scs_users_screen_name_un unique,
  priv_name integer default 0 
    constraint scs_users_priv_name_nn not null,
  priv_email integer default 5
    constraint scs_users_priv_email_nn not null,
  email_verified_p char(1) default 't'
    constraint scs_users_email_verified_p_ck check (email_verified_p in ('t', 'f')),
  email_bouncing_p char(1) default 'f'
    constraint scs_users_email_bouncing_p_nn not null
    constraint scs_users_email_bouncing_p_ck check (email_bouncing_p in ('t','f')),
  no_alerts_until date,
  last_visit date,
  second_to_last_visit date,
  n_sessions integer default 0
    constraint scs_users_n_sessions_nn not null,
  password_question varchar2(1000),
  password_answer varchar2(1000),
  state varchar(30) default 'authorized'
    constraint scs_users_state_nn not null
    constraint scs_users_state_ck 
      check(state in ('need_email_verification_and_admin_approv', 'need_admin_approv', 
                      'need_email_verification', 'rejected', 'authorized', 
                      'banned', 'deleted')),
  last_modified date default sysdate 
    constraint scs_users_last_mod_nn not null,
  modifying_user integer
    constraint scs_users_mod_user_fk references scs_users(user_id)
    constraint scs_users_mod_user_nn not null,
  deleted_p char(1) 
    constraint scs_users_deleted_p_nn not null
    constraint scs_users_deleted_p_ck check (deleted_p in ('t','f'))
);

/* ========================================
   table scs_user_preferences
======================================== */
create table scs_user_preferences (
  user_id integer
    constraint scs_user_prefs_user_id_nn not null
    constraint scs_user_prefs_user_id_fk references scs_users(user_id)
    constraint scs_user_prefs_pk primary key,
  -- an ISO 639 language code (in lowercase)
  language_pref	char(2) default 'en'
    constraint scs_user_prefs_lang_pref_nn not null
    constraint scs_user_prefs_lang_pref_fk references scs_lang(language),
  dont_spam_me_p char(1) default 'f'
    constraint user_prefs_dont_spam_me_p_ck check (dont_spam_me_p in ('t','f'))
);

-- used in the buffer table scs_user_imports 
create sequence scs_user_imports_id_seq;

/* ========================================
   table scs_user_imports

   This is a buffer containing new and updated information from
   external data sources. Entries are inserted in the tables
     scs_users, scs_persons, scs_person_rels
   using functions from the package 'scs_user'
======================================== */
create table scs_user_imports (
  user_imp_id integer
    constraint scs_user_imports_u_imp_id_nn not null
    constraint scs_user_imports_pk primary key,
  first_names varchar2(100),
  last_name varchar2(100),
  norm_name varchar2(200),
  security_id varchar2(50),
  email	varchar2(100),
  url varchar2(200),
  on_what_table varchar(100),
  on_which_id varchar(100),
  last_auto_import_try date,
  last_modified date default sysdate
    constraint scs_user_imps_l_mod_nn not null,
  modifying_user integer
    constraint scs_user_imps_l_mod_u_nn not null
    constraint scs_user_imps_l_mod_u_fk references scs_users(user_id)
);

comment on column scs_users.no_alerts_until is '
 For suppressing email alerts
';

comment on column scs_users.last_visit is '
 Set when user reappears at site
';

comment on column scs_users.second_to_last_visit is ' This is what
 most pages query against (since last_visit will only be a few minutes
 old for most pages in a session) ';

comment on column scs_users.n_sessions is ' How many times this user
 has visited ';


--------------
-- Triggers --
--------------
create or replace trigger scs_user_imports_in_up_tr
before insert or update on scs_user_imports
for each row
begin
  :new.norm_name := scs_person.norm_name(:new.first_names,:new.last_name);
  :new.email := lower(:new.email);
end;
/
show errors

-----------
-- Views --
-----------
create or replace view scs_users_active as
  select *
    from scs_users
   where deleted_p = 'f'
     and user_id <> 0;







