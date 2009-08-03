#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         "slsma.ss"
         "point.ss")

(define/provide-test-suite slsma-tests
  (test-case
   "filter-bounded-obstacle"
   (check-equal? (filter-bounded-obstacle
                  (vector (make-polar 10  0) (make-polar 10 .1) (make-polar 10 .2)
                          (make-polar 10 .1) (make-polar 10  0) (make-polar 10 .1)))
                 (vector (make-polar  10  0) (make-polar 10 .1) (make-polar 10 .1))))

  (test-case
   "filter-opaque removes points obscured by others in same set"
   (check-equal? (filter-opaque
                  (vector (make-polar 10 0) (make-polar 5 .5) (make-polar 10 .7)
                          (make-polar 10 2) (make-polar 5 .5) (make-polar 5 3))
                  (vector)
                  1)
                 (vector (make-polar 5 .5) (make-polar 10 2) (make-polar 5 .5) (make-polar 5 3))))

  (test-case
   "filter-opaque removes points obscured by new-pts"
   (check-equal? (filter-opaque
                  (vector (make-polar 10 0) (make-polar 5 2) (make-polar 10 3))
                  (vector (make-polar 5 1.5) (make-polar 5 2.5))
                  1)
                 (vector (make-polar 10 0) (make-polar 5 2))))
  )