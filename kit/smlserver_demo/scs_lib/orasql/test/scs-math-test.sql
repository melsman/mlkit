-- $Id$

set serveroutput on


begin
  scs_test.printl( '------------------------------' );
  scs_test.printl( 'testing ''scs_math'' package' );
  scs_test.printl( '------------------------------' );

  scs_test.printl( 'testing function ''min'':' );
  scs_test.testBool( 'min', 1,  scs_math.min(-3 ,-4) = -4 );
  scs_test.testBool( 'min', 2,  scs_math.min(-4 , 0)  = -4  );
  scs_test.testBool( 'min', 3,  scs_math.min(-3 , 5)  = -3 );
  scs_test.testBool( 'min', 4,  scs_math.min(0  , 4)   = 0  );
  scs_test.testBool( 'min', 5,  scs_math.min(10 , 5) = 5  );

  scs_test.testBool( 'min', 6,  scs_math.min(-3.0 ,-4.0) = -4.0 );
  scs_test.testBool( 'min', 7,  scs_math.min(-4.0 , 0.0)  = -4.0  );
  scs_test.testBool( 'min', 8,  scs_math.min(-3.0 , 5.0)  = -3.0 );
  scs_test.testBool( 'min', 9,  scs_math.min(0.0  , 4.0)   = 0.0  );
  scs_test.testBool( 'min', 10, scs_math.min(10.0 , 5.0) = 5.0  );

  scs_test.testBool( 'min', 11,  scs_math.min(-3 ,-4.0) = -4.0 );
  scs_test.testBool( 'min', 12,  scs_math.min(-4 , 0.0)  = -4  );
  scs_test.testBool( 'min', 13,  scs_math.min(-3 , 5.0)  = -3 );
  scs_test.testBool( 'min', 14,  scs_math.min(0  , 4.0)   = 0  );
  scs_test.testBool( 'min', 15, scs_math.min(10 , 5.0) = 5.0  );


  scs_test.printl( 'testing function ''max'':' );
  scs_test.testBool( 'max', 1,  scs_math.max(-3 ,-4) = -3 );
  scs_test.testBool( 'max', 2,  scs_math.max(-4 , 0)  = 0  );
  scs_test.testBool( 'max', 3,  scs_math.max(-3 , 5)  = 5 );
  scs_test.testBool( 'max', 4,  scs_math.max(0  , 4)   = 4  );
  scs_test.testBool( 'max', 5,  scs_math.max(10 , 5) = 10  );

  scs_test.testBool( 'max', 6,  scs_math.max(-3.0 ,-4.0) = -3.0 );
  scs_test.testBool( 'max', 7,  scs_math.max(-4.0 , 0.0)  = 0.0  );
  scs_test.testBool( 'max', 8,  scs_math.max(-3.0 , 5.0)  = 5.0 );
  scs_test.testBool( 'max', 9,  scs_math.max(0.0  , 4.0)   = 4.0  );
  scs_test.testBool( 'max', 10, scs_math.max(10.0 , 5.0) = 10.0  );

  scs_test.testBool( 'max', 11,  scs_math.max(-3 ,-4.0) = -3 );
  scs_test.testBool( 'max', 12,  scs_math.max(-4 , 0.0)  = 0.0  );
  scs_test.testBool( 'max', 13,  scs_math.max(-3 , 5.0)  = 5.0 );
  scs_test.testBool( 'max', 14,  scs_math.max(0  , 4.0)   = 4.0  );
  scs_test.testBool( 'max', 15, scs_math.max(10 , 5.0) = 10 );
end;
/
show errors


exec scs_test.printl( 'creating tmp tables...' );
create table tmp( 
  random integer
    constraint tmp_random_pk primary key
);
create table tmp2( 
  random integer
    constraint tmp2_random_pk primary key
    constraint tmp2_random_ck check (random <= 100000000000)
);
create table tmp3( 
  random varchar2(11)
    constraint tmp3_random_pk primary key
);

declare 
  i integer;
begin
  scs_test.printl( '------------------------------' );
  scs_test.printl( 'testing ''scs_random'' package' ); 
  scs_test.printl( '------------------------------' );

  scs_test.printl( 'testing function ''rand'':' );
  
  scs_test.testUnit( 'rand', 1, '
    declare
      i integer;
    begin
      for i in 1..10000 loop
	insert into tmp(random) values( scs_random.rand );
      end loop;
    end;' );
  
  scs_test.printl( 'testing function ''rand_max'':' );
  scs_test.testUnit( 'rand_max', 1, '
    declare
      i integer;
    begin
      for i in 1..10000 loop
	insert into tmp(random) values( scs_random.rand_max(100000000000) );
      end loop;
    end;' );

  scs_test.printl( 'testing function ''rand_string'':' );
  scs_test.testUnit( 'rand_string', 1, '
    declare
      i integer;
    begin
      for i in 1..10000 loop
	insert into tmp3(random) values( scs_random.rand_string( 11 ) );
      end loop;
    end;' );
end;
/
show errors

exec scs_test.printl( 'dropping tmp tables...' );
drop table tmp;
drop table tmp2;
drop table tmp3;


