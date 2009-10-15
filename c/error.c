#include "point.h"
#include "icp.h"
#include "imrp.h"
#include "error.h"

double normalised_error(polar_t ref_pts[], polar_t new_pts[], int n_pts, double xt, double yt, double a, double rotation) 
{
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
    
    if (p2.r == -1 && p2.a == -1) {
      error += 100;
    } else {
      polar_to_cartesian(p1, &c1);
      polar_to_cartesian(p2, &c2);
      dist = cartesian_distance(c1, c2);
      error += dist;
    }
    
    p2 = imrp_matches[i];
    
    if (p2.r == -1 && p2.a == -1) {
      error += 100;
    } else {
      polar_to_cartesian(p1, &c1);
      polar_to_cartesian(p2, &c2);
      dist = cartesian_distance(c1, c2);
      error += dist;
    }
  }

  return error / (2 * n_pts);
}
