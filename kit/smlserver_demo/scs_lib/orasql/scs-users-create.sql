-- This code i a down-sized and modified version of the users module found
-- in openACS (www.openacs.org): file community-core-create.sql.

create table scs_users (
  user_id integer
    constraint scs_users_user_id_nn not null
    constraint scs_users_user_id_fk references scs_persons(person_id)
    constraint scs_users_pk primary key,
  password char(40),
  salt char(40),
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
  last_modified date default sysdate 
    constraint scs_users_last_mod_nn not null,
  modifying_user integer
    constraint scs_users_mod_user_fk references scs_users(user_id)
    constraint scs_users_mod_user_nn not null,
  deleted_p char(1) 
    constraint scs_users_deleted_p_nn not null
    constraint scs_users_deleted_p_ck check (deleted_p in ('t','f'))
);

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

----------------------
-- SCS USER PACKAGE --
----------------------

create or replace package scs_user
as
 function new (
    user_id           in scs_users.user_id%TYPE default null,
    password          in scs_users.password%TYPE,
    salt              in scs_users.salt%TYPE,
    screen_name	      in scs_users.screen_name%TYPE default null,
    priv_name         in scs_users.priv_name%TYPE default 0,
    priv_email        in scs_users.priv_email%TYPE default 5,
    email_verified_p  in scs_users.email_verified_p%TYPE default 't',
    email_bouncing_p  in scs_users.email_bouncing_p%TYPE default 'f',
    password_question in scs_users.password_question%TYPE default null,
    password_answer   in scs_users.password_answer%TYPE default null,
    modifying_user    in scs_users.modifying_user%TYPE,
    email	      in scs_parties.email%TYPE,
    url               in scs_parties.url%TYPE default null,
    first_names	      in scs_persons.first_names%TYPE,
    last_name	      in scs_persons.last_name%TYPE,
    security_id       in scs_persons.security_id%TYPE default null,
    language_pref     in scs_user_preferences.language_pref%TYPE default 'en'
  ) return scs_users.user_id%TYPE;
 
  function receives_alerts_p (
    user_id in scs_users.user_id%TYPE
  ) return char;

  procedure approve_email (
    user_id in scs_users.user_id%TYPE
  );

  procedure unapprove_email (
    user_id in scs_users.user_id%TYPE
  );

  procedure delete (
    user_id in scs_users.user_id%TYPE
  );

  function system
  return scs_users.user_id%TYPE;
end scs_user;
/
show errors

-------------------------------------
-- SCS USER PACKAGE IMPLEMENTATION --
-------------------------------------

create or replace package body scs_user
as
  function new (
    user_id           in scs_users.user_id%TYPE default null,
    password          in scs_users.password%TYPE,
    salt              in scs_users.salt%TYPE,
    screen_name	      in scs_users.screen_name%TYPE default null,
    priv_name         in scs_users.priv_name%TYPE default 0,
    priv_email        in scs_users.priv_email%TYPE default 5,
    email_verified_p  in scs_users.email_verified_p%TYPE default 't',
    email_bouncing_p  in scs_users.email_bouncing_p%TYPE default 'f',
    password_question in scs_users.password_question%TYPE default null,
    password_answer   in scs_users.password_answer%TYPE default null,
    modifying_user    in scs_users.modifying_user%TYPE,
    email	      in scs_parties.email%TYPE,
    url               in scs_parties.url%TYPE default null,
    first_names	      in scs_persons.first_names%TYPE,
    last_name	      in scs_persons.last_name%TYPE,
    security_id       in scs_persons.security_id%TYPE default null,
    language_pref     in scs_user_preferences.language_pref%TYPE default 'en'
  ) return scs_users.user_id%TYPE
  is
    v_user_id scs_users.user_id%TYPE;
  begin
    v_user_id := 
      person.new(user_id, email, url, first_names, last_name, 
                 security_id, sysdate, modifying_user, 'f');

    insert into scs_users
      (user_id, password, salt, screen_name, priv_name, priv_email, email_verified_p, email_bouncing_p, 
       password_question, password_answer, last_modified, modifying_user, deleted_p)
    values
      (v_user_id, password, salt, screen_name, priv_name, priv_email, email_verified_p, email_bouncing_p,
       password_question, password_answer, sysdate, modifying_user, 'f');

    insert into scs_user_preferences (user_id, language_pref)
    values (v_user_id, language_pref);

    return v_user_id;
  end new;

  function receives_alerts_p (
    user_id in scs_users.user_id%TYPE
  ) return char
  is
    counter char(1);
  begin
    select decode(count(*),0,'f','t') into counter
      from scs_users
     where no_alerts_until >= sysdate
       and user_id = scs_user.receives_alerts_p.user_id;

    return counter;
  end receives_alerts_p;

  procedure approve_email (
    user_id in scs_users.user_id%TYPE
  )
  is
  begin
    update scs_users
       set email_verified_p = 't'
     where user_id = approve_email.user_id;
  end approve_email;

  procedure unapprove_email (
    user_id in scs_users.user_id%TYPE
  )
  is
  begin
    update scs_users
       set email_verified_p = 'f'
     where user_id = unapprove_email.user_id;
  end unapprove_email;

  procedure delete (
    user_id in scs_users.user_id%TYPE
  )
  is
  begin
    update scs_users
       set deleted_p = 't'
     where user_id = scs_user.delete.user_id;

    person.delete(user_id);
  end delete;

  function system
  return scs_users.user_id%TYPE
  is 
  begin
    -- We hard-code 1 as the site wide system user.
    return 1;
  end system;
end scs_user;
/
show errors

-------------------
-- SCS USER DATA --
-------------------

declare 
  uid scs_users.user_id%TYPE;
begin
  -- We hard code user_id 1 as being the site wide administrator
  uid := scs_user.new(user_id        => '1',
                      password       => 'change_me',
                      salt           => '12345666554321dlksaælfdsa',
                      modifying_user => '1',
                      email          => 'nh@it-c.dk',
                      first_names    => 'Site-wide SCS Administrator',
                      last_name      => 'SCS');

  -- We hard code user_id 0 as being the "not logged in" user
  uid := scs_user.new(user_id        => '0',
                      password       => '',
                      salt           => '',
                      modifying_user => '1',
                      email          => '',
                      first_names    => 'Anonymous User',
                      last_name      => 'Anonymous User');

end;
/ 
show errors
               
