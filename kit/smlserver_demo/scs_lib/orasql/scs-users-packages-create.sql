-- $Id$

-- This code i a down-sized and modified version of the users module found
-- in openACS (www.openacs.org): file community-core-create.sql.


/* ======================================================================
   package scs_user

   History:
   191102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   281002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_user
as
  /* ------------
     function new
     ------------
     creates a new user (and therefore also a new person and party)
     and sets its preferences

     throws a ScsDbExn exception if 
       *  an user_id is supplied and there exists an entry in scs_persons 
          with this id as person_id
       *  a password is supplied that exists in the table scs_party 
       *  a screen_name is supplied that exists in the table scs_users
  */
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

  /* -----------------
     procedure destroy
     -----------------
     for a valid user_id:
       sets the scs_users.deleted_p flag to 't'
       invalidates the screen_name
       calls scs_persons.destroy on the user_id
     for an invalid user_id nothing happens
     
     never fails
  */
  procedure destroy (
    user_id in scs_users.user_id%TYPE
  );

  /* ------------------
     function deleted_p
     ------------------
     returns the scs_users.deleted_p flag ('t'/'f')

     returns null if user_id is invalid     
  */
  function deleted_p (
    user_id in scs_users.user_id%TYPE
  ) return char;

  /* ----------------------
     function language_pref
     ----------------------
     returns the language preference for the user 

     returns null if user_id is invalid
  */
  function language_pref (
    user_id in scs_users.user_id%TYPE
  ) return scs_user_preferences.language_pref%TYPE;

  /* -------------------
     function gen_passwd
     -------------------
     generates and returns a password using email as salt

     returns null if provided with email = null

     You'd have to have the same first 4 characters in the email 
     and generate at the same time of day to get a duplicate.
     This is taken from:
     http://asktom.oracle.com/pls/ask/f?p=4950:8:::::F4950_P8_DISPLAYID:98812348060
  */
  function gen_passwd (
    email in varchar2
  ) return varchar2;

  /* ---------------
     function system
     ---------------
     returns the user_id for the system site administrator
     (hard coded in the file scs-default-users)
  */
  function system
  return scs_users.user_id%TYPE;

  /* ------------------------
     function imp_exact_match
     ------------------------
     returns the person_id if there is an exact match between  
     a row 'p_import' in the import table p_import and a row 'p' in the table
     scs_persons.

     returns null if there isn't an exact match.

     We say that p_import is an exact match of p if one of
     the following is true:

     1: The pair (p_import.on_what_table,p_import.on_which_id) is
        equal to (p.on_what_table,p.on_which_id)

     2: The security id p_import.security_id is equal to
        p.security_id.

     3: The pair (p_import.normalised_name,p_import.email) is equal to
        (p.normalised_name,p.email)
        
     Notice, that an exact match on email is not enough
  */
  function imp_exact_match (
    user_imp_id in scs_user_imports.user_imp_id%TYPE
  ) return scs_persons.person_id%TYPE;


  procedure imp_row_into_user (
    user_imp_id in scs_user_imports.user_imp_id%TYPE,
    user_id     in scs_users.user_id%TYPE
  );

  /* -----------------
     procedure imp_row
     -----------------
     1) if there is an exact match on user_imp_id as defined by the function
     'scs_user.imp_exact_match' then the following fields (if not null) 
     are read from the table user_imports and updated in the table scs_persons:
         first_names 
         last_name 
         security_id 
         email 
         url 
         (on_what_table, on_which_id)
     After the updates the import row is physically deleted

     or 

     2) if there is no exact match AND 
	   the normalised name for the import row is not null AND
           no other person has the same normalised name
     then a new user is created and the import row is physically deleted

     or

     2½) if there is no exact match AND 
            the normalised name for the import row is not null AND
            the flag 'always_p' = 't'
     then a new user is created (this might give duplicate normalised names 
     in scs_persons) and the import row is physically deleted

     or

     3) if 1, 2 or 2½ fail 
     then the field 'last_auto_import_try' is updated
     
     never fails
  */  
  procedure imp_row (
    user_imp_id in scs_user_imports.user_imp_id%TYPE,
    always_p    in char default 'f'
  );

  /* ----------------------
     procedure imp_all_rows
     ----------------------
     loops through all records in the table scs_user_imports
     and calls the procedure imp_row on each record
  */
  procedure imp_all_rows (
    always_p in char default 'f'
  );


/* function receives_alerts_p

   this function is implemented in the package body, but
   no test cases are provided
  function receives_alerts_p (
    user_id in scs_users.user_id%TYPE
  ) return char;
*/

/* procedure approve_email

   this procedure is implemented in the package body, but
   no test cases are provided
  procedure approve_email (
    user_id in scs_users.user_id%TYPE
  );
*/

/* procedure unapprove_email

   this procedure is implemented in the package body, but
   no test cases are provided
  procedure unapprove_email (
    user_id in scs_users.user_id%TYPE
  );
*/

end scs_user;
/
show errors


/* ======================================================================
   package bodies start here
====================================================================== */
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

    insert into scs_users(
      user_id, 
      password, 
      salt, 
      screen_name, 
      priv_name, 
      priv_email, 
      email_verified_p, 
      email_bouncing_p, 
      password_question, 
      password_answer, 
      last_modified, 
      modifying_user, 
      deleted_p )
    values(
      v_user_id, 
      password, 
      salt, 
      screen_name, 
      priv_name, 
      priv_email, 
      email_verified_p, 
      email_bouncing_p,
      password_question, 
      password_answer, 
      sysdate, 
      modifying_user, 
      'f' );

    insert into scs_user_preferences (user_id, language_pref)
    values (v_user_id, language_pref);

    return v_user_id;
  exception
    when others then
      raise_application_error( scs.ScsDbExn,
        'Cannot create user_id ' || user_id || ':' || SQLERRM );
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
    v_deleted_p		char(1);
    v_screen_name	scs_users.screen_name%TYPE;
  begin
    select screen_name, deleted_p into v_screen_name, v_deleted_p
    from scs_users
    where scs_users.user_id = scs_user.destroy.user_id;

    if( v_deleted_p = 'f' ) then
      update scs_users
         set deleted_p = 't',
             screen_name = scs.invalidate_field( v_screen_name, 100, user_id )
       where user_id = scs_user.destroy.user_id;
    end if;

    scs_person.destroy(user_id);
  exception
    when NO_DATA_FOUND then
      return ;
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
  exception
    when NO_DATA_FOUND then
      return null;
  end deleted_p;

  function language_pref (
    user_id in scs_users.user_id%TYPE
  ) return scs_user_preferences.language_pref%TYPE
  is
    v_lang_pref scs_user_preferences.language_pref%TYPE;
  begin
    select language_pref
      into v_lang_pref
      from scs_user_preferences
     where user_id = language_pref.user_id;

    return v_lang_pref;
  exception
    when NO_DATA_FOUND then
      return null;
  end language_pref;

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
    -- We have hard-coded 1 as the site wide system user.
    return 1;
  end system;


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

  procedure imp_row_into_user (
    user_imp_id in scs_user_imports.user_imp_id%TYPE,
    user_id     in scs_users.user_id%TYPE
  )
  is
    row scs_user_imports%ROWTYPE;
  begin
    select *
      into imp_row_into_user.row
      from scs_user_imports
     where scs_user_imports.user_imp_id = imp_row_into_user.user_imp_id;

    /* first_names */
    if imp_row_into_user.row.first_names is not null then
      update scs_persons
         set first_names = imp_row_into_user.row.first_names
       where scs_persons.person_id = imp_row_into_user.user_id;
    end if;
    /* last_name */
    if imp_row_into_user.row.last_name is not null then
      update scs_persons
         set last_name = imp_row_into_user.row.last_name
       where scs_persons.person_id = imp_row_into_user.user_id;
    end if;
    /* security_id */
    if imp_row_into_user.row.security_id is not null then
      update scs_persons
         set security_id = imp_row_into_user.row.security_id
       where scs_persons.person_id = imp_row_into_user.user_id;
    end if;
    /* email */
    if imp_row_into_user.row.email is not null then
      update scs_parties
         set email = imp_row_into_user.row.email
       where scs_parties.party_id = imp_row_into_user.user_id;
    end if;
    /* url */
    if imp_row_into_user.row.url is not null then
      update scs_parties
         set url = imp_row_into_user.row.url
       where scs_parties.party_id = imp_row_into_user.user_id;
    end if;
    /* on_what_table and on_which_id */
    if imp_row_into_user.row.on_what_table is not null and 
       imp_row_into_user.row.on_which_id is not null then
      scs_person_rel.add(person_id => imp_row_into_user.user_id,
                         on_what_table => imp_row_into_user.row.on_what_table,
                         on_which_id => imp_row_into_user.row.on_which_id);
    end if;

    /* Delete imported row */
    delete from scs_user_imports
     where scs_user_imports.user_imp_id = imp_row_into_user.row.user_imp_id;
  exception
    when no_data_found then
      return; /* We silently ignore that the import row does not exist. */
  end imp_row_into_user;

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

