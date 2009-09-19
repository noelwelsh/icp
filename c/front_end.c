#include <stdio.h>
#include <stdlib.h>

#include "idc.h"
#include "icp.h"
#include "imrp.h"
#include "point.h"

polar_t scans[59][180];
void init_scans();

double normalised_error(polar_t ref_pts[], polar_t new_pts[], double xt, double yt, double a, double rotation) 
{
  int n_pts = 180;
  polar_t transformed_pts[n_pts];

  for (int i = 0; i < n_pts; i++) {
    polar_t p;
    cartesian_t c, c1;
    
    polar_to_cartesian(new_pts[i], &c);
    cartesian_transform(c, xt, yt, a, &c1);
    cartesian_to_polar(c1, &p);
    
    transformed_pts[i] = p;
  }

  polar_t icp_matches[n_pts];
  polar_t imrp_matches[n_pts];
  
  icp_matching_points(ref_pts, transformed_pts, n_pts, rotation, icp_matches);
  imrp_matching_points(ref_pts, transformed_pts, n_pts, rotation, imrp_matches);
  
  double error = 0.0;
  for(int i = 0; i < n_pts; i++) 
  {
    cartesian_t c1, c2;
    polar_t p1, p2;
    double dist;
    
    p1 = transformed_pts[i];
    p2 = icp_matches[i];
    
    polar_to_cartesian(p1, &c1);
    polar_to_cartesian(p2, &c2);
    dist = cartesian_distance(c1, c2);
    error += (dist * dist);

    p2 = imrp_matches[i];
    
    polar_to_cartesian(p2, &c2);
    dist = cartesian_distance(c1, c2);
    error += (dist * dist);
  }

  return error;
}


int main(void) 
{
  init_scans();
  double xt, yt, a;

  for(int i = 0; i < 59; i++) 
  {
    for(int j = 0; j < 59; j++) 
    {
      polar_t *ref_pts = scans[i];
      polar_t *new_pts = scans[j];
      
      idc(ref_pts, new_pts, 180,
          0.0, 0.0, 0.0, 0.2,
          100, 0.00001,
          &xt, &yt, &a);

      printf("SCAN MATCH %d %d %lf %lf %lf\n", i, j, xt, yt, a);
      double error = normalised_error(ref_pts, new_pts, xt, yt, a, 0.2);
      printf("ERROR %d %d %lf\n", i, j, error);
    }
  }

  return EXIT_SUCCESS;
}

