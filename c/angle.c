#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "angle.h"

double two_pi = 2 * M_PI;


double angle_normalise(double a) 
{
  if (0 <= a && a < two_pi) {
    return a;
  } else if (a < 0) {
    return angle_normalise(a - (floor(a / two_pi) * two_pi));
  } else if (two_pi <= a){
    return angle_normalise(a - (ceil(a / two_pi) * two_pi));
  } else {
    printf("angle_normalise given non-numeric %lf angle.\n", a);
    exit(EXIT_FAILURE);
  }
}

bool angle_less_than(double a1, double a2) 
{
  if (angle_normalise(a2 - a1) < M_PI) {
    return true;
  } else {
    return false;
  }
}
