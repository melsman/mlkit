       local
         fun f'(p as (0,b)) = p
           | f'(n,b) = f'(n-1,n*b)
       in
         fun f(a,b) = #2(f'(a,b))
       end;

