#include "stdio.h"

double putchard(double X) {
  putchar((char)X);
  fflush(stdout);
  return 0;
}

double printd(double X) {
  printf("%f\n", X);
  return 0;
}
