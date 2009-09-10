#ifndef __IMRP_H_
#define __IMRP_H_

#include "point.h"

double imrp_closest_point(polar_t, polar_t, polar_t, polar_t *);
void imrp_interpolate_point_to_angle(polar_t, polar_t, double, polar_t *);
void imrp_interpolate_point_to_range(polar_t, polar_t, double, polar_t *);
void imrp_matching_points(polar_t[], polar_t[], int, double, polar_t[]);



#endif /* __IMRP_H_ */
