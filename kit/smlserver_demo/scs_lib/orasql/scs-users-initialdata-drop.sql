begin
  scs_role.destroy(abbreviation => 'ScsPersonAdm');
end;
/
show errors

commit;
