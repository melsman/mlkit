create table scs_country_codes(
  country_id integer
    constraint scs_country_c_con_id_nn not null
    constraint scs_country_c_con_id_pk primary key,
  abbreviation varchar2(4)
    constraint scs_country_c_abbr_nn not null,
  abbreviation_tmp varchar2(4),
  country_name_tid integer
    constraint scs_country_c_con_name_tid_nn not null
    constraint scs_country_c_con_id_fk references scs_texts(text_id),
  country_name_tmp_dk varchar2(50),
  country_name_tmp_en varchar2(50),
  abbr_iso2 varchar2(4),
--    constraint scs_country_c_abbr_iso2_nn not null,
  abbr_iso3 varchar2(4),
--    constraint scs_country_c_abbr_iso3_nn not null,
  nationalitet_dk varchar2(50)
);

