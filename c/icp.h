#ifndef __ICP_H_
#define __ICP_H_

#include "point.h"

double icp_closest_point(polar_t, polar_t, polar_t, polar_t *);
void icp_interpolate_point_to_angle(polar_t, polar_t, double, polar_t *);


#endif /* __ICP_H_ */
