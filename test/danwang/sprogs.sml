structure SProgs =
  struct
    val L = SObjCPS.Lambda
    fun A x y = SObjCPS.Apply(x,y)
    fun C x y = SObjCPS.Cons(x,y)
    val V = SObjCPS.V
    val I = SObjCPS.I
    val N = SObjCPS.Nil
    open SObjCPS

    val Y = (L (fn f =>
		(A
		 (L (fn x => (A (V f)
			      (L (fn a => (A (A (V x) (V x)) (V a)))))))
		 (L (fn x => (A (V f)
			      (L (fn a => (A (A (V x) (V x)) (V a))))))))))

    val int_list =
      (C (I 1) (C (I 2) (C (I 3) (C (I 4) N))))

    val it_rev_list =
      (A Y (L (fn it_rev =>
           (L (fn l => (L (fn acc =>
	    (If (NilP (V l),(V acc),
	        (A (A (V it_rev) (Cdr (V l))) (C (Car (V l)) (V acc))))))))))))
			       
    fun mk_list 0 = N
      | mk_list n = (C (I n) (mk_list (n - 1)))
    val rev_list = (L (fn l => (A (A it_rev_list (V l)) Nil)))


  end
