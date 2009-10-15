#ifndef __ERROR_H_
#define __ERROR_H_

#include "point.h"

double normalised_error(polar_t ref_pts[], polar_t new_pts[], int n_pts, double xt, double yt, double a, double rotation);



#endif /* __ERROR_H_ */
