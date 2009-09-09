#include "icp.h"
#include "geometry.h"
#include "point.h"
#include "angle.h"

double icp_closest_point(polar_t pt, polar_t pt1, polar_t pt2, polar_t *out)
{
  double dist;
  cartesian_t c, c1, c2, cout;

  polar_to_cartesian(pt, &c);
  polar_to_cartesian(pt1, &c1);
  polar_to_cartesian(pt2, &c2);
  
  dist = line_segment_closest_point(c1, c2, c, &cout);
  cartesian_to_polar(cout, out);

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

