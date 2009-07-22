#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/vector)
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

  (test-case
   "matching-points returns matchs in correct order"
   (define pts1 (vector (make-point 2 3) (make-point 4 2) (make-point 3 1)))
   (define pts2 (vector (make-point 2 3) (make-point 4 2) (make-point 3 1)))
   (check-equal? (matching-points pts1 pts2 1)
                 pts2))

  (test-case
   "optimal-transformation finds correct rotation"
   ;; Points on a square (or, a circle!)
   (define scan-pts (vector (make-point 3 0) (make-point 3 (/ pi 2))
                            (make-point 3 pi) (make-point 3 (* pi 3/2))))
   ;; Same points rotated by a constant amount
   (define matching-pts (vector (make-point 3 .2) (make-point 3 (+ (/ pi 2) .2))
                            (make-point 3 (+ pi .2)) (make-point 3 (+ (* pi 3/2) .2))))
   (define-values (tx ty a)
     (optimal-transformation scan-pts matching-pts))
   (check-= a 0.2 0.00001))

  (test-case
   "optimal-transformation finds correct translation"
   ;; Points on a square (or, a circle!)
   (define scan-pts (vector (make-point 3 0) (make-point 3 (/ pi 2))
                            (make-point 3 pi) (make-point 3 (* pi 3/2))))
   ;; Same points translated by a constant amount
   (define matching-pts (vector-map cartesian->polar
                                    (vector (make-point 3.1 .1) (make-point .1 3.1)
                                            (make-point -2.9 .1) (make-point .1 -2.9))))
   (define-values (tx ty a)
     (optimal-transformation scan-pts matching-pts))
   (check-= tx 0.1 0.00001)
   (check-= ty 0.1 0.00001))
  )