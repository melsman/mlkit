-- $Id$

load data

infile	    'hsas_landekoder_med_iso.csv'
badfile	    'hsas_landekoder_med_iso.bad'
discardfile 'hsas_landekoder_med_iso.dis'

append into table scs_country_codes_w
fields terminated by ';'
(
  abbreviation char,
  country_name_tmp_dk char,
  country_name_tmp_en char,
  forvalg filler char,
  blank filler char,
  abbreviation_tmp char,
  navn filler char,
  eng_navn filler char,
  abbr_iso2 char,
  abbr_iso3 char ,
  nationalitet_dk char
)
