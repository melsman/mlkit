int toML(int x) {
  return 2 * x + 1;
}

int toC(int x) {
  return x >> 1;
}

int shift_lefti(int i, int n) {
  return 1 + ((i - 1) << (n >> 1));
}

int shift_right_signedi(int i, int n) {
  return 1 | ((i) >> (n >> 1));
}

int shift_right_unsignedi(int i, int n) {
  return  1 | ((unsigned long)(i) >> (n >> 1));
}

int main() {
  printf("shift_lefti(2,1) = %d\n", toC(shift_lefti(toML(2),toML(1))));
  printf("shift_lefti(2,2) = %d\n", toC(shift_lefti(toML(2),toML(2))));
  printf("shift_right_signedi(8,1) = %d\n", toC(shift_right_signedi(toML(8),toML(1))));
  printf("shift_right_signedi(8,2) = %d\n", toC(shift_right_signedi(toML(8),toML(2))));
  printf("shift_right_signedi(5,1) = %d\n", toC(shift_right_signedi(toML(5),toML(1))));
  printf("shift_right_signedi(5,2) = %d\n", toC(shift_right_signedi(toML(5),toML(2))));
  printf("shift_right_signedi(5,3) = %d\n", toC(shift_right_signedi(toML(5),toML(3))));

  printf("shift_right_unsignedi(8,1) = %d\n", toC(shift_right_unsignedi(toML(8),toML(1))));
  printf("shift_right_unsignedi(8,2) = %d\n", toC(shift_right_unsignedi(toML(8),toML(2))));
  printf("shift_right_unsignedi(5,1) = %d\n", toC(shift_right_unsignedi(toML(5),toML(1))));
  printf("shift_right_unsignedi(5,2) = %d\n", toC(shift_right_unsignedi(toML(5),toML(2))));
  printf("shift_right_unsignedi(5,3) = %d\n", toC(shift_right_unsignedi(toML(5),toML(3))));

  return 0;
}
