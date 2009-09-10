#include "imrp.h"
#include "geometry.h"
#include "point.h"
#include "angle.h"
#include "icp_base.h"

double imrp_closest_point(polar_t pt, polar_t pt1, polar_t pt2, polar_t *out)
{
  double r, a, r1, a1, r2, a2;
  cartesian_t c1, c2;

  r  = pt.r;
  a  = pt.a;
  
  r1 = pt1.r;
  r2 = pt2.r;
  a1 = pt1.a;
  a2 = pt2.a;

  if ( (r1 <=  r) && (r2 <= r) ) {
    if (r1 > r2) {
      out->r = r1;
      out->a = a1;
    } else {
      out->r = r2;
      out->a = a2;
    }
  } else if ( (r1 > r) && (r2 > r) ) {
    if (r1 < r2) {
      out->r = r1;
      out->a = a1;
    } else {
      out->r = r2;
      out->a = a2;
    }
  } else {
    imrp_interpolate_point_to_range(pt1, pt2, r, out);
  }
  
  polar_to_cartesian(pt, &c1);
  polar_to_cartesian(*out, &c2);
  
  return cartesian_distance(c1, c2);
}

void imrp_interpolate_point_to_angle(polar_t pt1, polar_t pt2, double a, polar_t *out) 
{
  double r, r1, a1, r2, a2;
  
  r1 = pt1.r;
  r2 = pt2.r;
  a1 = pt1.a;
  a2 = pt2.a;

  r =
    ((r1 * r2 * (angle_normalise(a2 - a1))) /
     ((r1 * (angle_normalise(a - a1))) + (r2 * (angle_normalise(a2 - a)))));
  
  out->r = r;
  out->a = a;

  return;
}

void imrp_interpolate_point_to_range(polar_t pt1, polar_t pt2, double r, polar_t *out) 
{
  double r1, r2, a1, a2;
  double a;

  r1 = pt1.r;
  r2 = pt2.r;
  a1 = pt1.a;
  a2 = pt2.a;
  
  a = ((1 / (r1 - r2)) *
       (((r1 * r2 * (angle_normalise(a2 - a1))) / r) +
        ((r1 * a1) - (r2 * a2))));
  
  out->r = r;
  out->a = angle_normalise(a);

  return;
}

void imrp_matching_points(polar_t new_pts[], polar_t ref_pts[], int n_pts, double r, polar_t matching_pts[])
{
  matching_points(new_pts, n_pts, ref_pts, n_pts, r,
                  &imrp_interpolate_point_to_angle, &imrp_closest_point, 
                  matching_pts);
  
  return;
}
