#include <stdlib.h>
#include <math.h>

#include "angle.h"
#include "point.h"

/*
 * TYPES
 */


typedef void (*interpolate_point_to_angle_t) (polar_t*, polar_t*, double, polar_t*);
typedef double (*closest_point_t) (polar_t*, polar_t*, polar_t*, polar_t*);



/*
 * PROTOTYPES
 */
void matching_points(polar_t[], int, polar_t[], int, double, interpolate_point_to_angle_t, closest_point_t, polar_t[]);
void matching_point(polar_t, polar_t[], int, double, interpolate_point_to_angle_t, closest_point_t, polar_t*);





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
    matching_point(pt, model_pts, n_model_pts, rotation, interpolate_point_to_angle, closest_point, &(matching_pts[i]));
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
  
  r = pt.r;
  a = pt.a;
  
  low = angle_normalise(a - rotation);
  high = angle_normalise(a + rotation);
  
  found = false;
  found_dist = -HUGE_VAL;

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
      break;
    } else { 
      if (angle_less_than(a1, low)) {
        interpolate_point_to_angle(&pt1, &pt2, low, &low_pt);
      } else {
        low_pt = pt1;
      }

      if (angle_less_than(high, a2)) {
        interpolate_point_to_angle(&pt1, &pt2, high, &high_pt);
      } else {
        high_pt = pt2;
      }

      closest_dist = closest_point(&pt, &low_pt, &high_pt, &closest);

      if (!found) {
        (*found_pt).r = closest.r;
        (*found_pt).a = closest.a;
        
        found_dist = closest_dist;
      } else if (closest_dist <= found_dist) {
        (*found_pt).r = closest.r;
        (*found_pt).a = closest.a;

        found_dist = closest_dist;
      }
    }
  }
  
}

