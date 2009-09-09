#include "util.h"

double sse1(double err[], int n) 
{
  double squares, sum;
  int i;
  double x;

  squares = 0;
  sum = 0;

  for(i = 0; i < n; i++) 
  {
    x = err[i];
    squares += (x * x);
    sum += x;
  }
  
  return squares - (sum * sum / n);
}

double sse2(double err1[], double err2[], int n) 
{
  double sum_xy, sum_x, sum_y, mean_x, mean_y;
  int i;
  double x, y;

  sum_xy = 0;
  sum_x = 0;
  sum_y = 0;
  mean_x = 0;
  mean_y = 0;

  for(i = 0; i < n; i++) 
  {
    x = err1[i];
    y = err2[i];

    sum_xy += x * y;
    sum_x  += x;
    sum_y  += y;
  }

  mean_x = sum_x / n;
  mean_y = sum_y / n;
  
  return sum_xy - (mean_x * sum_y) - (mean_y * sum_x) + (n * mean_x * mean_y);
}

