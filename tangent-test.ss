#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "tangent.ss"
         "point.ss")

(define/provide-test-suite tangent-tests
  (test-case
   "fit-tangent finds straight line"
   ;; Points along a straight line with no noise
   (check-equal?
    (fit-tangent (vector (make-cartesian 0 3) (make-cartesian -3 3)
                         (make-cartesian -2 3) (make-cartesian -1 3)
                         (make-cartesian 0 3) (make-cartesian 1 3)
                         (make-cartesian 2 3) (make-cartesian 3 3)))
    (make-polar 3 (/ pi 2))))

  (test-case
   "fit-tangents returns the correct number of points"
   (define pts (fit-tangents
                (vector (make-cartesian 0 3) (make-cartesian -3 3)
                        (make-cartesian -2 3) (make-cartesian -1 3)
                        (make-cartesian 0 3) (make-cartesian 1 3)
                        (make-cartesian 2 3) (make-cartesian 3 3))
                3))
   (check-equal? (vector-length pts) 6))
  )