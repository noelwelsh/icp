#include <math.h>
#include "point.h"
#include "pose.h"
#include "util.h"

double line_segment_closest_point(cartesian_t pt1, cartesian_t pt2, cartesian_t pt,
                                  cartesian_t* out) 
{
  double u, dist;

  if (cartesian_equal(pt1, pt2))
  {
    out->x = pt1.x;
    out->y = pt1.y;
    
    return cartesian_distance(pt1, pt);
  } else {
    double x1, x2, y1, y2, x, y;
    x = pt.x;
    y = pt.y;
    x1 = pt1.x;
    y1 = pt1.y;
    x2 = pt2.x;
    y2 = pt2.y;
    
    dist = cartesian_distance(pt2, pt1);
    u = (x - x1) * (x2 - x1) + (y - y1) * (y2 - y1);
    u = u / (dist * dist);
    
    if (u <= 0) 
    {
      out->x = pt1.x;
      out->y = pt1.y;
    } else if (1 <= u) 
    {
      out->x = pt2.x;
      out->y = pt2.y;
    } else 
    {
      out->x = pt1.x + (u * (pt2.x - pt1.x));
      out->y = pt1.y + (u * (pt2.y - pt1.y));
    }
    
    return cartesian_distance(*out, pt);
  }
}

void line_line_intersection(cartesian_t pt1, cartesian_t pt2, cartesian_t pt3, cartesian_t pt4, cartesian_t *out) 
{
  double x1, x2, y1, y2, x3, y3, x4, y4;
  
  x1 = pt1.x;
  y1 = pt1.y;
  x2 = pt2.x;
  y2 = pt2.y;
  x3 = pt3.x;
  y3 = pt3.y;
  x4 = pt4.x;
  y4 = pt4.y;

  // This formula taken from Wikipedia
  out->x =
    (((((x1 * y2) - (y1 * x2)) * (x3 - x4)) -
      ((x1 - x2) * ((x3 * y4) - (y3 * x4))))
     /
     (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4))));

  out->y =
    (((((x1 * y2) - (y1 * x2)) * (y3 - y4)) -
      ((y1 - y2) * ((x3 * y4) - (y3 * x4))))
     /
     (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4))));
  
  return;
}

