declare 
  role_id integer;
begin
  -- ScsPersonAdm is the role giving administrator rights to the Central Personnel Register
  role_id := scs_role.new(abbreviation => 'ScsPersonAdm',
                          role_description_tid => 
                            scs_text.updateText(language => 'da',
                                                text => 'Centralt Personregister Administrator',
                                                language2 => 'en',
                                                text2 => 'Central Personnel Register Administrator'));
end;
/
show errors

commit;
