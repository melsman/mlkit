-- $Id$

-- This code i a down-sized and modified version of the party module found
-- in openACS (www.openacs.org): file community-core-create.sql.



/* ======================================================================
   package scs_party

   functionality for party manipulation

   History:
   151102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   091002 Niels Hallenberg <nh@it.edu> created package
====================================================================== */
create or replace package scs_party
as

  scs_portrait_types constant varchar(30) := 'scs_portrait_types';

  /* ------------
     function new
     ------------
     creates a new party
     Raises ScsDbExn exception if there exists an entry with email 'email'
  */
  function new (
    party_id	   in scs_parties.party_id%TYPE default null,
    party_type     in scs_parties.party_type%TYPE default null,
    email	   in scs_parties.email%TYPE default null,
    url		   in scs_parties.url%TYPE default null,
    last_modified  in scs_parties.last_modified%TYPE default sysdate,
    modifying_user in scs_parties.modifying_user%TYPE default null,
    deleted_p      in scs_parties.deleted_p%TYPE default 'f'
  ) return scs_parties.party_id%TYPE;

  /* -----------------
     procedure destroy
     -----------------
     sets deleted_p = 't' and invalidates email for a party entry

     never fails
  */
  procedure destroy (
    party_id in scs_parties.party_id%TYPE
  );

  /* --------------
     function email
     --------------
     returns the email for a party_id
     Returns null if party_id is illegal
  */
  function email (
    party_id in scs_parties.party_id%TYPE
  ) return scs_parties.email%TYPE;

  /* --------------
     function partyIdByEmail
     --------------
     returns the party id for the party with the argument
     email. Returns null if such an email does not exists. Email is
     unique.  */
  function partyIdByEmail (
    email in scs_parties.email%TYPE
  ) return scs_parties.party_id%TYPE;

  /* --------------
     function url
     --------------
     returns the url for a party_id
     Throws a ScsDbExn exception if party_id is illegal
  */
  function url (
    party_id in scs_parties.party_id%TYPE
  ) return scs_parties.url%TYPE;

end scs_party;
/
show errors

create or replace package body scs_party
as
  function new (
    party_id	   in scs_parties.party_id%TYPE default null,
    party_type     in scs_parties.party_type%TYPE default null,
    email	   in scs_parties.email%TYPE default null,
    url		   in scs_parties.url%TYPE default null,
    last_modified  in scs_parties.last_modified%TYPE default sysdate,
    modifying_user in scs_parties.modifying_user%TYPE default null,
    deleted_p      in scs_parties.deleted_p%TYPE default 'f'
  )
  return scs_parties.party_id%TYPE
  is
    v_party_id scs_parties.party_id%TYPE;
  begin
    v_party_id := scs.new_obj_id(party_id);

    insert into scs_parties (party_id, party_type, email, url)
      values (v_party_id, party_type, lower(email), url);

    return v_party_id;
  exception
    when DUP_VAL_ON_INDEX then
      raise_application_error( scs.ScsDbExn, 
			       'email exists: '|| new.email );
  end new;

  procedure destroy (
    party_id in scs_parties.party_id%TYPE
  )
  is
    v_deleted_p		char(1);
    v_email		varchar2(100);
  begin
    select deleted_p, email into v_deleted_p, v_email
    from scs_parties
    where scs_parties.party_id = scs_party.destroy.party_id;

    if( v_deleted_p = 'f' ) then
      update scs_parties
         set deleted_p = 't',
             email = scs.invalidate_field( v_email, 100, party_id )
       where scs_parties.party_id = scs_party.destroy.party_id;
    end if;
  exception
    when NO_DATA_FOUND then
      return ;
  end destroy;

  function partyIdByEmail (
    email in scs_parties.email%TYPE
  ) return scs_parties.party_id%TYPE
  is
    v_party_id scs_parties.party_id%TYPE;
  begin
    select party_id
      into v_party_id
      from scs_parties
     where scs_parties.email = partyIdByEmail.email
       and scs_parties.deleted_p = 'f';

    return v_party_id;

  exception 
    when others then
      return null;
  end partyIdByEmail;

  function email (
    party_id in scs_parties.party_id%TYPE
  ) return scs_parties.email%TYPE
  is
    v_email scs_parties.email%TYPE;
  begin
    select email
      into v_email
      from scs_parties
     where party_id = email.party_id;

    return v_email;
 exception 
   when NO_DATA_FOUND then
     return null;
   when others then
     raise_application_error(scs.ScsDbExn, 
        		     'some error on party with id '|| email.party_id );
 end email;

  function url (
    party_id in scs_parties.party_id%TYPE
  ) return scs_parties.url%TYPE
  is
    v_url scs_parties.url%TYPE;
  begin
    select url
      into v_url
      from scs_parties
     where party_id = url.party_id;

    return v_url;
 exception 
   when NO_DATA_FOUND then
     raise_application_error( scs.ScsDbExn, 
			       'no party with id '|| url.party_id );
 end url;

end scs_party;
/
show errors

-- DRB: I added this trigger to enforce the storing of e-mail in lower
-- case.  party.new() already did so but I found an update that
-- didn't...  Also, we makes sure that email is never null. This may
-- happen otherwise because we do not always know email-addresses on
-- persons we create.
create or replace trigger scs_parties_in_up_tr
before insert or update on scs_parties
for each row
begin
  :new.email := lower(:new.email);
  if :new.email is null then
    :new.email := :new.party_id;
  end if;
end;
/
show errors

