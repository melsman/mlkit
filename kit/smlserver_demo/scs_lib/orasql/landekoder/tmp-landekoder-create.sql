create view scs_country_codes_w (  
  abbreviation,
  abbreviation_tmp,
  country_name_tmp_dk ,
  country_name_tmp_en ,
  abbr_iso2,
  abbr_iso3,
  nationalitet_dk
  ) as 
  select abbreviation, abbreviation_tmp,
	 country_name_tmp_dk ,
	 country_name_tmp_en ,
	 abbr_iso2,
	 abbr_iso3,
	 nationalitet_dk
    from scs_country_codes;


create or replace function check_lande_kode(
  LANDEKODE in varchar2 ,
  KODE	    in varchar2 
) return varchar2
is
  not_eq exception;
begin
  if LANDEKODE = KODE then
    return LANDEKODE;
  else
    raise not_eq;
  end if;
end check_lande_kode;
/
show errors

create or replace trigger scs_country_codes_tr
instead of insert on scs_country_codes_w
declare
  tid integer;
begin
  tid := scs_text.updateText(
	text_id => null,
	language => 'da',
	text => :new.country_name_tmp_dk ,
	language2 => 'en',
	text2 => :new.country_name_tmp_en
  );

  insert into scs_country_codes(
      country_id,
      abbreviation,
      country_name_tid,
      abbr_iso2 ,
      abbr_iso3 ,
      nationalitet_dk 
  ) values (
      scs.new_obj_id,
      check_lande_kode(:new.abbreviation, :new.abbreviation_tmp),
      tid,
      :new.abbr_iso2 ,
      :new.abbr_iso3 ,
      :new.nationalitet_dk
  );
end scs_country_codes_tr;
/
show errors
