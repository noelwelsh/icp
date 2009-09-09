#ifndef __ICP_BASE_H_
#define __ICP_BASE_H_

#include "point.h"

typedef void (*interpolate_point_to_angle_t) (polar_t, polar_t, double, polar_t*);
typedef double (*closest_point_t) (polar_t, polar_t, polar_t, polar_t*);

void matching_points(polar_t[], int, polar_t[], int, double, interpolate_point_to_angle_t, closest_point_t, polar_t[]);
void matching_point(polar_t, polar_t[], int, double, interpolate_point_to_angle_t, closest_point_t, polar_t*);

#endif /* __ICP_BASE_H_ */
