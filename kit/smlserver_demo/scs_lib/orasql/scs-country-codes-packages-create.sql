-- $Id$

/* ======================================================================
   package scs_country_code

   provides functionality for looking up country names

   History:
   2004-04-21 Kennie Nybo Pontoppidan <kennie@it-c.dk> created package
====================================================================== */ 

create or replace package scs_country_code
as

  /* ----------------
     function getName
     ----------------
     returns the string representation of country_id in language lang

     returns null if country_id is unknown or null
  */
  function getName(
    country_id in scs_country_codes.country_id%TYPE,
    lang       in scs_lang.language%TYPE
  ) return varchar2;
end scs_country_code;
/
show errors


create or replace package body scs_country_code
as

  function getName(
    country_id in scs_country_codes.country_id%TYPE,
    lang       in scs_lang.language%TYPE
  ) return varchar2
  is
    tid integer;
  begin
    select country_name_tid into tid
      from scs_country_codes
     where country_id = getName.country_id;

    return scs_text.getText (tid, lang);
  exception
    when NO_DATA_FOUND then return null;
  end getName;
  
end scs_country_code;
/
show errors
