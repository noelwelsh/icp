#include <math.h>

#include "idc.h"
#include "imrp.h"
#include "icp.h"
#include "point.h"
#include "pose.h"

void idc(polar_t ref_pts[], polar_t new_pts[], int n_pts,
         double xt, double yt, double a, double rotation,
         int iterations, double threshold,
         double out[3])
//         double *xt_out, double *yt_out, double *a_out) 
{
  for(int i = 0; i < iterations; i++) 
  {
    double d_xt, d_yt, d_a;
    double xt1, yt1, a1, xt2, yt2, a2;
    
    icp(ref_pts, new_pts, n_pts, xt, yt, a, rotation, &xt1, &yt1, &a1);
    imrp(ref_pts, new_pts, n_pts, xt, yt, a, rotation, &xt2, &yt2, &a2);
    
    d_xt = xt1;
    d_yt = yt1;
    d_a = a2;
    
    if ( (fabs(d_xt) < threshold) && (fabs(d_yt) < threshold) && (fabs(d_a) < threshold) ) 
    {
      break;
    } else {
      xt += d_xt;
      yt += d_yt;
      a += d_a;
    }
  }

//  *xt_out = xt;
//  *yt_out = yt;
//  *a_out = a;
  out[0] = xt;
  out[1] = yt;
  out[2] = a;
  
  return;
}

