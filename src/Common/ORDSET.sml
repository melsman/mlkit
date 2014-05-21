(*$ORDSET*)
signature ORDSET = 
sig
  type item
  val lt: item * item -> bool
  
  type StringTree
  val show: item-> StringTree
end
