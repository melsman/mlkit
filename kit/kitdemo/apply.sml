       fun apply f x = f x
       val y = apply (fn n => n + 1.0) 5.0
       val z = apply (fn m => m) 6
