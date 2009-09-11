#include <stdlib.h>
#include <stdio.h>

#include "idc.h"
#include "imrp.h"
#include "icp.h"
#include "point.h"
#include "math.h"

#define n_pts 180
#define n_iterations 1000

int main(void) 
{
  polar_t ref_pts[n_pts];
  polar_t new_pts[n_pts];
  double xt, yt, a;
  unsigned short int xsubi[3] = {1, 7, 36};
    
  for(int i = 0; i < n_iterations; i++) 
  {
    for(int j = 0; j < n_pts; j++) 
    {
      ref_pts[j].r = erand48(xsubi) * 500.0 + 10.0;
      new_pts[j].r = erand48(xsubi) * 500.0 + 10.0;
      
      ref_pts[j].a = j / (2 * M_PI);
      new_pts[j].a = j / (2 * M_PI);
    }
    
    //imrp(new_pts, ref_pts, n_pts, 0.0, 0.0, 0.0, 0.2, &xt, &yt, &a);
    //printf("IMRP Iteration %d: %lf %lf %lf\n", i, xt, yt, a);

    //icp(new_pts, ref_pts, n_pts, 0.0, 0.0, 0.0, 0.2, &xt, &yt, &a);
    //printf("ICP Iteration %d: %lf %lf %lf\n", i, xt, yt, a);

    idc(new_pts, ref_pts, n_pts, 0.0, 0.0, 0.0, 0.2, 100, 0.001, &xt, &yt, &a);
    printf("IDC Iteration %d: %lf %lf %lf\n", i, xt, yt, a);

  }
  
  return EXIT_SUCCESS;
}

