-- This code i a down-sized and modified version of the users module found
-- in openACS (www.openacs.org): file community-core-create.sql.

-- The field state below follows the Phil'g registration cyclus - yet
-- to be implemented, 2002-10-20, nh.
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

create sequence scs_user_imports_id_seq;
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
    email	      in scs_parties.email%TYPE default null,
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

  procedure destroy (
    user_id in scs_users.user_id%TYPE
  );

  function deleted_p (
    user_id in scs_users.user_id%TYPE
  ) return char;

  function gen_passwd (
    email in varchar2
  ) return varchar2;

  function system
  return scs_users.user_id%TYPE;

  function imp_exact_match (
    user_imp_id in scs_user_imports.user_imp_id%TYPE
  ) return scs_persons.person_id%TYPE;

  procedure imp_row (
    user_imp_id in scs_user_imports.user_imp_id%TYPE,
    always_p    in char default 'f'
  );

  procedure imp_all_rows (
    always_p in char default 'f'
  );
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
    email	      in scs_parties.email%TYPE default null,
    url               in scs_parties.url%TYPE default null,
    first_names	      in scs_persons.first_names%TYPE,
    last_name	      in scs_persons.last_name%TYPE,
    security_id       in scs_persons.security_id%TYPE default null,
    language_pref     in scs_user_preferences.language_pref%TYPE default 'da'
  ) return scs_users.user_id%TYPE
  is
    v_user_id scs_users.user_id%TYPE;
  begin
    v_user_id := 
      scs_person.new(user_id, email, url, first_names, last_name, 
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
    select decode(count(*),0,'f','t') 
      into counter
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

  procedure destroy (
    user_id in scs_users.user_id%TYPE
  )
  is
  begin
    update scs_users
       set deleted_p = 't',
           screen_name = user_id || '-' || screen_name
     where user_id = scs_user.destroy.user_id;

    scs_person.destroy(user_id);
  end destroy;

  function deleted_p (
    user_id in scs_users.user_id%TYPE
  ) return char
  is
    v_deleted_p char;
  begin
    select deleted_p
      into v_deleted_p
      from scs_users
     where scs_users.user_id = deleted_p.user_id;
  
    return v_deleted_p;
  end deleted_p;

  /* You'd have to have the same first 4 digits and do it at the same
    time of day to get a dup.
    This is taken from:
    http://asktom.oracle.com/pls/ask/f?p=4950:8:::::F4950_P8_DISPLAYID:98812348060
  */
  function gen_passwd (
    email in varchar2
  ) return varchar2
  is
    j      number := 0;
    k      number;
    str    varchar2(30);
    result varchar2(30);
  begin
    if email is null then
      return null;
    end if;
    str := substr (email, 1, 4) || to_char (sysdate, 'SSSS');
    for i in 1 .. least (length (str), 8)
    loop
      j := mod (j + ascii (substr (str, i, 1)), 256);
      k := mod (bitand (j, ascii (substr (str, i, 1))), 74) + 48;
      if k between 58 and 64 then
        k := k + 7;
      elsif k between 91 and 96 then
        k := k + 6;
      end if;
      result := result || chr (k);
    end loop;
    result := replace (result, '1', '2');
    result := replace (result, 'l', 'L');
    result := replace (result, '0', '9');
    result := replace (result, 'O', 'P');
    result := 'A' || substr (result, 2);
    return result;
  end gen_passwd;

  function system
  return scs_users.user_id%TYPE
  is 
  begin
    -- We hard-code 1 as the site wide system user.
    return 1;
  end system;

  /* Given a row in the import table p_import and a row in the person
     table p. We say that p_import is an exact match of p if one of
     the following is true:

     1: The pair (p_import.on_what_table,p_import.on_which_id) is
        equal to (p.on_what_table,p.on_which_id)

     2: The security id p_import.security_id is equal to
        p.security_id.

     3: The pair (p_import.normalised_name,p_import.email) is equal to
        (p.normalised_name,p.email)
        
        Notice, that an exact email match is not enough
  */
  function imp_exact_match (
    user_imp_id in scs_user_imports.user_imp_id%TYPE
  ) return scs_persons.person_id%TYPE
  is
    v_person_id scs_person_rels.person_id%TYPE;
  begin
    begin
      select scs_person_rels.person_id
        into v_person_id
        from scs_user_imports, scs_person_rels
       where scs_user_imports.on_what_table = scs_person_rels.on_what_table    /* 1 */
         and scs_user_imports.on_which_id = scs_person_rels.on_which_id
         and scs_user_imports.on_what_table is not null
         and scs_user_imports.on_which_id is not null
         and scs_user_imports.user_imp_id = imp_exact_match.user_imp_id;
      return v_person_id;
    exception
      when no_data_found then
      begin
        select scs_persons.person_id
          into v_person_id
          from scs_persons, scs_user_imports
         where scs_persons.security_id = scs_user_imports.security_id          /* 2 */
           and scs_user_imports.user_imp_id = imp_exact_match.user_imp_id
           and scs_user_imports.security_id is not null
           and scs_persons.deleted_p = 'f';
        return v_person_id;
      exception
        when no_data_found then
        begin
          select scs_persons.person_id
            into v_person_id
            from scs_persons, scs_user_imports, scs_parties
           where scs_user_imports.norm_name = scs_persons.norm_name            /* 3 */
             and scs_user_imports.email = scs_parties.email
             and scs_user_imports.email is not null
             and scs_user_imports.norm_name is not null
             and scs_persons.person_id = scs_parties.party_id
             and scs_user_imports.user_imp_id = imp_exact_match.user_imp_id
             and scs_persons.deleted_p = 'f';
          return v_person_id;
        exception
          when no_data_found then
            return null;
        end;
      end;
    end;
  end imp_exact_match;

  procedure imp_row (
    user_imp_id in scs_user_imports.user_imp_id%TYPE,
    always_p    in char default 'f'
  )
  is
    v_person_id scs_persons.person_id%TYPE;
    row scs_user_imports%ROWTYPE;
    tmp_id scs_users.user_id%TYPE;
  begin
    select *
      into imp_row.row
      from scs_user_imports
     where scs_user_imports.user_imp_id = imp_row.user_imp_id;

    v_person_id := scs_user.imp_exact_match(imp_row.user_imp_id);

    if v_person_id is not null then
      /* We update fields */
      begin
        /* first_names */
        if imp_row.row.first_names is not null then
          update scs_persons
             set first_names = imp_row.row.first_names
           where scs_persons.person_id = v_person_id;
        end if;
        /* last_name */
        if imp_row.row.last_name is not null then
          update scs_persons
             set last_name = imp_row.row.last_name
           where scs_persons.person_id = v_person_id;
        end if;
        /* security_id */
        if imp_row.row.security_id is not null then
          update scs_persons
             set security_id = imp_row.row.security_id
           where scs_persons.person_id = v_person_id;
        end if;
        /* email */
        if imp_row.row.email is not null then
          update scs_parties
             set email = imp_row.row.email
           where scs_parties.party_id = v_person_id;
        end if;
        /* url */
        if imp_row.row.url is not null then
          update scs_parties
             set url = imp_row.row.url
           where scs_parties.party_id = v_person_id;
        end if;
        /* on_what_table and on_which_id */
        if imp_row.row.on_what_table is not null and 
           imp_row.row.on_which_id is not null then
          scs_person_rel.add(person_id => v_person_id,
                             on_what_table => imp_row.row.on_what_table,
                             on_which_id => imp_row.row.on_which_id);
        end if;

        /* Delete imported row */
        delete from scs_user_imports
         where scs_user_imports.user_imp_id = imp_row.row.user_imp_id;
      end;
    else
      /* We create a new user, if no other person has the same normalised name */
      begin
        if imp_row.row.norm_name is not null and 
           (scs_person.norm_name_exists_p(imp_row.row.norm_name) = 'f' or
           always_p = 't') then
          /* No other row exsits with the same normalised name which is non empty */
          tmp_id := scs_user.new(password => scs_user.gen_passwd(imp_row.row.email),
                                 salt => scs_random.rand_string(30),
                                 modifying_user => imp_row.row.modifying_user,
                                 email => imp_row.row.email,
                                 url => imp_row.row.url,
                                 first_names => imp_row.row.first_names,
                                 last_name => imp_row.row.last_name,
                                 security_id => imp_row.row.security_id,
                                 language_pref => 'da');

          /* on_what_table and on_which_id */
          if imp_row.row.on_what_table is not null and imp_row.row.on_which_id is not null then
            scs_person_rel.add(person_id => tmp_id,
                               on_what_table => imp_row.row.on_what_table,
                               on_which_id => imp_row.row.on_which_id);
          end if;

          /* Delete imported row */
          delete from scs_user_imports
           where scs_user_imports.user_imp_id = imp_row.row.user_imp_id;
        else
          /* Mark row in import-table that it could not be imported at this time */
          update scs_user_imports
             set last_auto_import_try = sysdate
           where scs_user_imports.user_imp_id = imp_row.row.user_imp_id;
        end if;
      end;
    end if;
  exception
    when no_data_found then
      return; /* We silently ignore that the import row does not exist. */
  end imp_row;

  procedure imp_all_rows (
    always_p in char default 'f'
  )
  is
    r scs_user_imports%ROWTYPE;
  begin
    for r in (select * 
                from scs_user_imports
               order by user_imp_id)
    loop
      scs_user.imp_row(user_imp_id => r.user_imp_id,
                       always_p => always_p);
    end loop;
  end imp_all_rows;
end scs_user;
/
show errors

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
                      email          => 'siteadm@it-c.dk',
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
