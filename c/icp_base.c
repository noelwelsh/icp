#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#include "angle.h"
#include "point.h"
#include "util.h"
#include "icp_base.h"

/*********************************************************
 
 By convention a failed match is denoted by a polar point
 with radius and angle both -1.

 *********************************************************/


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
        low_pt.r = pt1.r;
        low_pt.a = pt1.a;
      }

      if (angle_less_than(high, a2)) {
        interpolate_point_to_angle(pt1, pt2, high, &high_pt);
      } else {
        high_pt.r = pt2.r;
        high_pt.a = pt2.a;
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

void optimal_transformation(polar_t ref_pts[], polar_t matching_pts[], int n_pts, double* xt, double* yt, double* a) 
{
  int n_actual_matches = 0;
  
  for (int i = 0; i < n_pts; i++) {
    polar_t pt = matching_pts[i];
    if ( (pt.r == -1) && (pt.a == -1) ) {
      continue;
    } else {
      n_actual_matches++;
    }
  }
  
  if ( (n_actual_matches == 0) || (n_actual_matches == 1) ) {
    printf("icp_base: Zero or one matching points. Returning no transformation.\n");

    *xt = 0.0;
    *yt = 0.0;
    *a  = 0.0;
    return;
  } else {
    int idx = 0;
    cartesian_t c, cm;
    double r_x_mean, r_y_mean, m_x_mean, m_y_mean;
    double r_xs[n_actual_matches];
    double r_ys[n_actual_matches];
    double m_xs[n_actual_matches];
    double m_ys[n_actual_matches];

    r_x_mean = r_y_mean = m_x_mean = m_y_mean = 0.0;
    
    for (int i = 0; i < n_pts; i++) {
      polar_t pt = ref_pts[i];
      polar_t match_pt = matching_pts[i];

      if ( (match_pt.r == -1) && (match_pt.a == -1) ) {
        continue;
      } else {
        polar_to_cartesian(pt, &c);
        polar_to_cartesian(match_pt, &cm);
        
        r_xs[idx] = c.x;
        r_ys[idx] = c.y;
        m_xs[idx] = cm.x;
        m_ys[idx] = cm.y;
        
        r_x_mean += c.x;
        r_y_mean += c.y;
        m_x_mean += cm.x;
        m_y_mean += cm.y;
        
        idx++;
      }
    }
 
    r_x_mean /= n_actual_matches;
    r_y_mean /= n_actual_matches;
    m_x_mean /= n_actual_matches;
    m_y_mean /= n_actual_matches;
      
    double Sxx = sse2(r_xs, m_xs, n_actual_matches);
    double Syy = sse2(r_ys, m_ys, n_actual_matches);
    double Sxy = sse2(r_xs, m_ys, n_actual_matches);
    double Syx = sse2(r_ys, m_xs, n_actual_matches);
    
    if ( (Sxx == 0.0) && (Syy == 0.0) ) {
      printf("icp_base: Sxx and Sxy both zero. Returning no transformation.\n");
      
      *xt = 0.0;
      *yt = 0.0;
      *a  = 0.0;
      return;
    } else {
      double angle = atan((Sxy - Syx) / (Sxx + Syy));
      *xt = (m_x_mean -  (r_x_mean * cos(angle)) - (r_y_mean * sin(angle)));
      *yt = (m_y_mean - (r_x_mean * sin(angle)) - (r_y_mean * cos(angle)));
      *a = angle;
      
      return;
    }
  }
}

