#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#include "angle.h"
#include "point.h"
#include "icp_base.h"

void matching_points(polar_t scan_pts[], int n_scan_pts,
                     polar_t model_pts[], int n_model_pts,
                     double rotation,
                     interpolate_point_to_angle_t interpolate_point_to_angle,
                     closest_point_t closest_point,
                     polar_t matching_pts[]) 
{
  int i;
  polar_t pt;
  
  for(i = 0; i < n_scan_pts; i++) 
  {
    pt = scan_pts[i];
    matching_point(pt, model_pts, n_model_pts, rotation, interpolate_point_to_angle, closest_point, matching_pts + i);
  }
  
  return;
}


void matching_point(polar_t pt, polar_t pts[], int n_pts, double rotation,
                    interpolate_point_to_angle_t interpolate_point_to_angle,
                    closest_point_t closest_point,
                    polar_t *found_pt) 
{
  int i, found;
  double r, a;
  double low, high;
  polar_t closest, pt1, pt2, low_pt, high_pt;
  double found_dist, closest_dist;
  double r1, r2, a1, a2;
  
  //printf("Searching for match for point (%lf,%lf)\n", pt.r, pt.a);

  r = pt.r;
  a = pt.a;
  
  low = angle_normalise(a - rotation);
  high = angle_normalise(a + rotation);
  
  found = false;
  found_dist = HUGE_VAL;

  found_pt->r = -1.0;
  found_pt->a = -1.0;
  
  for(i = 0; i < (n_pts - 1); i++) 
  {
    pt1 = pts[i];
    pt2 = pts[i+1];
    
    r1 = pt1.r;
    a1 = pt1.a;
    r2 = pt2.r;
    a2 = pt2.a;
    
    if ( (angle_less_than(a1, low) && angle_less_than(a2, low)) ||
         (angle_less_than(high, a1) && angle_less_than(high, a2)) ) {
      // Skip
      //printf("Skipping pair (%lf,%lf) and (%lf,%lf)\n", r1, a1, r2, a2);
      continue;
    } else { 
      if (angle_less_than(a1, low)) {
        interpolate_point_to_angle(pt1, pt2, low, &low_pt);
      } else {
        low_pt = pt1;
      }

      if (angle_less_than(high, a2)) {
        interpolate_point_to_angle(pt1, pt2, high, &high_pt);
      } else {
        high_pt = pt2;
      }

      closest_dist = closest_point(pt, low_pt, high_pt, &closest);

      if (!found) {
        //printf("Found match found at point (%lf,%lf)\n", closest.r, closest.a);

        found_pt->r = closest.r;
        found_pt->a = closest.a;
        
        found_dist = closest_dist;
        found = true;
      } else if (closest_dist <= found_dist) {
        //printf("Found match found at point (%lf,%lf)\n", closest.r, closest.a);

        found_pt->r = closest.r;
        found_pt->a = closest.a;

        found_dist = closest_dist;
      }
    }
  }
  
  //if (!found) 
  //{
  //  printf("No match found for point (%lf,%lf)\n", pt.r, pt.a);
  //}

  return;
}

