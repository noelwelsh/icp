#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         "imrp.ss"
         "point.ss")

(define/provide-test-suite imrp-tests
  (test-case
   "interpolate-point-to-angle"
   (define p1 (make-point 2 -1))
   (define p2 (make-point 4 1))
   (check-equal?
    (interpolate-point-to-angle p1 p2 0)
    (make-point 8/3 0)))

  (test-case
   "interpolate-point-to-range"
   (define p1 (make-point 2 -1))
   (define p2 (make-point 4 1))
   (check-equal?
    (interpolate-point-to-range p1 p2 3)
    (make-point 3 1/3)))

  
  (test-case
   "closest-point, both ranges less"
   (define p (make-point 5 0))
   (define p1 (make-point 2 -1))
   (define p2 (make-point 4 1))
   (check-equal? (closest-point p p1 p2)
                 p2))

  (test-case
   "closest-point, both ranges greater"
   (define p (make-point 1 0))
   (define p1 (make-point 2 -1))
   (define p2 (make-point 4 1))
   (check-equal? (closest-point p p1 p2)
                 p1))

  (test-case
   "closest-point, ranges straddle point"
   (define p (make-point 3 0))
   (define p1 (make-point 2 -1))
   (define p2 (make-point 4 1))
   (check-equal? (closest-point p p1 p2)
                 (make-point 3 1/3)))

  (test-case
   "matching-point"
   (define p (make-point 3 0))
   (define pts (vector (make-point 3 3) (make-point 4 1) (make-point 2 -1) (make-point 3 -3)))
   (check-equal? (matching-point p pts 1)
                 (make-point 3 1/3)))
  )