#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/vector)
         "imrp.ss"
         "point.ss"
         "geometry.ss")

(define/provide-test-suite imrp-tests
  (test-case
   "interpolate-point-to-angle"
   (define p1 (make-polar 2 -1))
   (define p2 (make-polar 4 1))
   (check-equal?
    (interpolate-point-to-angle p1 p2 0)
    (make-polar 8/3 0)))

  (test-case
   "interpolate-point-to-range"
   (define p1 (make-polar 2 -1))
   (define p2 (make-polar 4 1))
   (check-equal?
    (interpolate-point-to-range p1 p2 3)
    (make-polar 3 1/3)))

  
  (test-case
   "closest-point, both ranges less"
   (define p (make-polar 5 0))
   (define p1 (make-polar 2 -1))
   (define p2 (make-polar 4 1))
   (check-equal? (closest-point p p1 p2)
                 p2))

  (test-case
   "closest-point, both ranges greater"
   (define p (make-polar 1 0))
   (define p1 (make-polar 2 -1))
   (define p2 (make-polar 4 1))
   (check-equal? (closest-point p p1 p2)
                 p1))

  (test-case
   "closest-point, ranges straddle point"
   (define p (make-polar 3 0))
   (define p1 (make-polar 2 -1))
   (define p2 (make-polar 4 1))
   (check-equal? (closest-point p p1 p2)
                 (make-polar 3 1/3)))

  (test-case
   "matching-point"
   (define p (make-polar 3 0))
   (define pts (vector (make-polar 3 3) (make-polar 4 1) (make-polar 2 -1) (make-polar 3 -3)))
   (check-equal? (matching-point p pts 1)
                 (make-polar 3 1/3)))

  (test-case
   "matching-points returns matchs in correct order"
   (define pts1 (vector (make-polar 2 3) (make-polar 4 2) (make-polar 3 1)))
   (define pts2 (vector (make-polar 2 3) (make-polar 4 2) (make-polar 3 1)))
   (check-equal? (matching-points pts1 pts2 1)
                 pts2))

  (test-case
   "optimal-transformation finds correct rotation"
   ;; Points on a square (or, a circle!)
   (define scan-pts (vector (make-polar 3 0) (make-polar 3 (/ pi 2))
                            (make-polar 3 pi) (make-polar 3 (* pi 3/2))))
   ;; Same points rotated by a constant amount
   (define matching-pts (vector (make-polar 3 .2) (make-polar 3 (+ (/ pi 2) .2))
                            (make-polar 3 (+ pi .2)) (make-polar 3 (+ (* pi 3/2) .2))))
   (define-values (tx ty a)
     (optimal-transformation scan-pts matching-pts))
   (check-= a 0.2 0.00001))

  (test-case
   "optimal-transformation finds correct translation"
   ;; Points on a square (or, a circle!)
   (define scan-pts (vector (make-polar 3 0) (make-polar 3 (/ pi 2))
                            (make-polar 3 pi) (make-polar 3 (* pi 3/2))))
   ;; Same points translated by a constant amount
   (define matching-pts (vector-map
                         cartesian->polar
                         (vector (make-cartesian 3.1 .1) (make-cartesian .1 3.1)
                                 (make-cartesian -2.9 .1) (make-cartesian .1 -2.9))))
   (define-values (tx ty a)
     (optimal-transformation scan-pts matching-pts))
   (check-= tx 0.1 0.00001)
   (check-= ty 0.1 0.00001))

  (test-case
   "Iterations of imrp converge to true transform for an ellipse"
   (define ref-pts (vector-map cartesian->polar (make-ellipse-points 5 5 40 20 .10 .1)))
   (define new-pts (vector-map cartesian->polar (make-ellipse-points 6 6 40 20 .40 .1)))
   (define-values (true-tx true-ty true-a) (values -1 -1 -.3))
   (define-values (tx ty a rotation)
     (for/fold ([tx -1] [ty -1] [a -.3] [rotation 0])
         ([i (in-range 10)])
       (define-values (next-tx next-ty next-a next-rotation)
         (let-values (([ntx nty na] (imrp ref-pts new-pts tx ty a rotation)))
           (values (+ tx ntx) (+ ty nty) (+ a na) (abs na))))
       ;;(printf "IMRP Iteration ~a: ~a ~a ~a ~a\n" i next-tx next-ty next-a next-rotation)
       '(check <
              (cartesian-distance (make-cartesian next-tx next-ty) (make-cartesian true-tx true-ty))
              (cartesian-distance (make-cartesian tx ty) (make-cartesian true-tx true-ty)))
       '(check < (abs (- next-r true-r)) (abs (- r true-r)))
       (values next-tx next-ty next-a next-rotation)))
   #t)
  )