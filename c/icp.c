#include "icp.h"
#include "geometry.h"
#include "point.h"
#include "angle.h"
#include "icp_base.h"

double icp_closest_point(polar_t pt, polar_t pt1, polar_t pt2, polar_t *out)
{
  double dist;
  cartesian_t c, c1, c2, cout;

  polar_to_cartesian(pt, &c);
  polar_to_cartesian(pt1, &c1);
  polar_to_cartesian(pt2, &c2);
  
  dist = line_segment_closest_point(c1, c2, c, &cout);
  cartesian_to_polar(cout, out);
  
  out->a = angle_normalise(out->a);
  
  return dist;
}

void icp_interpolate_point_to_angle(polar_t pt1, polar_t pt2, double a, polar_t *out) 
{
  polar_t ptemp;
  cartesian_t c1, c2, ctemp1, ctemp2, cout;

  polar_to_cartesian(pt1, &c1);
  polar_to_cartesian(pt2, &c2);

  ptemp.r = 1;
  ptemp.a = a;
  polar_to_cartesian(ptemp, &ctemp1);
  
  ctemp2.x = 0;
  ctemp2.y = 0;

  line_line_intersection(c1, c2, ctemp1, ctemp2, &cout);
  cartesian_to_polar(cout, out);
  out->a = angle_normalise(out->a);
}

void icp_interpolate_point_to_range(polar_t pt1, polar_t pt2, double r, polar_t *out) 
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

void icp_matching_points(polar_t new_pts[], polar_t ref_pts[], int n_pts, double r, polar_t matching_pts[])
{
  matching_points(new_pts, n_pts, ref_pts, n_pts, r,
                  &icp_interpolate_point_to_angle, &icp_closest_point, 
                  matching_pts);
  
  return;
}