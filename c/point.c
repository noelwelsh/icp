#include <math.h>
#include "point.h"
#include "angle.h"

void polar_to_cartesian(polar_t in, cartesian_t* out) 
{
  double x, y;
  x = in.r * cos(in.a);
  y = in.r * sin(in.a);
  
  out->x = x;
  out->y = y;
  
  return;
}

void cartesian_to_polar(cartesian_t in, polar_t* out) 
{
  double r, a;
  
  r = sqrt(in.x * in.x + in.y * in.y);
  a = (in.x == 0 && in.y == 0) ? 0.0 : atan2(in.y, in.x);
  
  out->r = r;
  out->a = a;
  
  return;
}

void cartesian_add(cartesian_t in1, cartesian_t in2, cartesian_t* out) 
{
  out->x = in1.x + in2.x;
  out->y = in1.y + in2.y;
  
  return;
}

void cartesian_minus(cartesian_t in1, cartesian_t in2, cartesian_t* out) 
{
  out->x = in1.x - in2.x;
  out->y = in1.y - in2.y;
  
  return;
}

double cartesian_distance(cartesian_t in1, cartesian_t in2) 
{
  cartesian_t diff;
  
  cartesian_minus(in1, in2, &diff);
  
  return sqrt(diff.x * diff.x + diff.y * diff.y);
}

double cartesian_dot(cartesian_t in1, cartesian_t in2)
{
  return in1.x * in2.x + in1.y * in2.y;
}

void cartesian_transform(cartesian_t in, double xt, double yt, double a, cartesian_t* out) 
{
  double cosa, sina;
  cosa = cos(a);
  sina = sin(a);

  out->x = (cosa * in.x) + xt - (sina * in.y);
  out->y = (sina * in.x) + yt + (cosa * in.y);
  
  return;
}

bool cartesian_equal(cartesian_t in1, cartesian_t in2) 
{
  return in1.x == in2.x && in1.y == in2.y;
}


void polar_add(polar_t in1, polar_t in2, polar_t* out) 
{
  cartesian_t temp1, temp2, temp3;

  polar_to_cartesian(in1, &temp1);
  polar_to_cartesian(in2, &temp2);
  cartesian_add(temp1, temp2, &temp3);
  cartesian_to_polar(temp3, out);

  return;
}

void polar_minus(polar_t in1, polar_t in2, polar_t* out) 
{
  cartesian_t temp1, temp2, temp3;

  polar_to_cartesian(in1, &temp1);
  polar_to_cartesian(in2, &temp2);
  cartesian_minus(temp1, temp2, &temp3);
  cartesian_to_polar(temp3, out);

  return;
}

double polar_dot(polar_t in1, polar_t in2)
{
  return in1.r * in2.r * cos(in1.a - in2.a);
}

void polar_rotate(polar_t in, double a, polar_t* out)
{
  out->r = in.r;
  out->a = in.a + a;

  return;
}

void polar_normalise(polar_t in, polar_t* out) 
{
  out->r = in.r;
  out->a = angle_normalise(in.a);
  
  return;
}
