void 
work() {
  int b;
  int a = 8;
  printf("Calling ML function \"incr\"\n");
  b = incr(a);
  printf("incr(%d) = %d\n", a,b);
  printf("Calling ML function \"incr\" again\n");
  b = incr(a);
  printf("incr(%d) = %d\n", a,b);
}
