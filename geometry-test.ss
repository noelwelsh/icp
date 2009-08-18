#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         "geometry.ss"
         "point.ss")

(define-check (check-closest-point pt1 pt2 pt expected-pt dist)
  (define e 0.00001)
  (define-values (closest-pt closest-dist) (line-segment-closest-point pt1 pt2 pt))
  (check-point closest-pt expected-pt e)
  (check-= closest-dist dist e))

(define/provide-test-suite geometry-tests
  (test-case
   "line-segment-closest-point, coincident pt1 and pt2"
   (check-closest-point (make-cartesian 0 0) (make-cartesian 0 0)
                        (make-cartesian 0 4)
                        (make-cartesian 0 0) 4))

  (test-case
   "line-segment-closest-point, end points are closest"
   (check-closest-point (make-cartesian 2 1) (make-cartesian 3 4)
                        (make-cartesian 0 0)
                        (make-cartesian 2 1) (sqrt 5))
   (check-closest-point (make-cartesian -2 1) (make-cartesian -1 1)
                        (make-cartesian 0 0)
                        (make-cartesian -1 1) (sqrt 2)))

  (test-case
   "line-segment-closest-point, normal case"
   (check-closest-point (make-cartesian -3 3) (make-cartesian 3 3)
                        (make-cartesian 0 0)
                        (make-cartesian 0 3) 3))

  (test-case
   "line-segment-closest-point, point intersects segment"
   (check-closest-point (make-cartesian -3 3) (make-cartesian 3 3)
                        (make-cartesian 0 3)
                        (make-cartesian 0 3) 0))
  )