#include <stdio.h>
#include <fftw3.h>

main () {
  printf("Size of fftw_complex: %d\n", sizeof (fftw_complex));
  printf("Size of fftw_plan: %d\n", sizeof (fftw_plan));
}
