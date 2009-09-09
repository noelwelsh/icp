#ifndef __POINT_H_
#define __POINT_H_

#include<stdbool.h>

typedef struct polar {
  double r;
  double a;
} polar_t;

typedef struct cartesian {
  double x;
  double y;
} cartesian_t;


void polar_to_cartesian(polar_t, cartesian_t*);
void cartesian_to_polar(cartesian_t, polar_t*);

void cartesian_add(cartesian_t, cartesian_t, cartesian_t*);
void cartesian_minus(cartesian_t, cartesian_t, cartesian_t*);
bool cartesian_equal(cartesian_t, cartesian_t);
double cartesian_distance(cartesian_t , cartesian_t);
double cartesian_dot(cartesian_t , cartesian_t);
void cartesian_transform(cartesian_t in, double xt, double yt, double a, cartesian_t*);

void polar_add(polar_t, polar_t, polar_t*);
void polar_minus(polar_t, polar_t, polar_t*);
double polar_dot(polar_t, polar_t);
void polar_rotate(polar_t, double, polar_t*);
void polar_normalise(polar_t, polar_t*);


#endif /* __POINT_H_ */
