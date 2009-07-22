#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         "slsma.ss"
         "point.ss")

(define/provide-test-suite slsma-tests
  (test-case
   "filter-bounded-obstacle"
   (check-equal? (filter-bounded-obstacle
                  (vector (make-point 10  0) (make-point 10 .1) (make-point 10 .2)
                          (make-point 10 .1) (make-point 10  0) (make-point 10 .1)))
                 (vector (make-point  10  0) (make-point 10 .1) (make-point 10 .1))))

  (test-case
   "filter-opaque removes points obscured by others in same set"
   (check-equal? (filter-opaque
                  (vector (make-point 10 0) (make-point 5 .5) (make-point 10 .7)
                          (make-point 10 2) (make-point 5 .5) (make-point 5 3))
                  (vector)
                  1)
                 (vector (make-point 5 .5) (make-point 10 2) (make-point 5 .5) (make-point 5 3))))

  (test-case
   "filter-opaque removes points obscured by new-pts"
   (check-equal? (filter-opaque
                  (vector (make-point 10 0) (make-point 5 2) (make-point 10 3))
                  (vector (make-point 5 1.5) (make-point 5 2.5))
                  1)
                 (vector (make-point 10 0) (make-point 5 2))))
  )