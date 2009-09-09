#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "point.ss")

(define e 0.00001)

(define/provide-test-suite point-tests
  (test-case
   "polar->cartesian"
   (check-point (polar->cartesian (make-polar 3 0))
                (make-cartesian 3 0)
                e)
   (check-point (polar->cartesian (make-polar 3 (* pi -3/4)))
                (make-cartesian (- (sqrt 4.5)) (- (sqrt 4.5)))
                e)
   (check-point (polar->cartesian (make-polar 3 (* pi 3/4)))
                (make-cartesian (- (sqrt 4.5)) (sqrt 4.5))
                e)
   (check-point (polar->cartesian (make-polar 3 (/ pi 4)))
                (make-cartesian (sqrt 4.5) (sqrt 4.5))
                e)
   (check-point (polar->cartesian (make-polar 3 (/ pi -4)))
                (make-cartesian (sqrt 4.5) (- (sqrt 4.5)))
                e))

  (test-case
   "cartesian->polar and polar->cartesian are inverses"
   (define points
     (list (make-cartesian 0 0) 
           (make-cartesian 10 10) (make-cartesian 3 0) (make-cartesian 0 3)
           (make-cartesian -1 -1) (make-cartesian -3 0) (make-cartesian 0 -3)
           (make-cartesian 100 1) (make-cartesian 1 100)))
   (for-each
    (lambda (p)
      (define p-prime (polar->cartesian (cartesian->polar p)))
      (check-= (cartesian-x p-prime) (cartesian-x p) e
               (format "x coordinate of point ~a/~a" p-prime p))
      (check-= (cartesian-y p-prime) (cartesian-y p) e
               (format "y coordinate of point ~a/~a" p-prime p)))
    points))
  
  (test-case
   "cartesian+ and cartesian-"
   (define pts1
     (list (make-cartesian 0 3)
           (make-cartesian 10 10)
           (make-cartesian 3 2)
           (make-cartesian 1 3)))
   (define pts2
     (list (make-cartesian 10 10)
           (make-cartesian 1 3)
           (make-cartesian 0 3)
           (make-cartesian 3 2)))
   (define added-pts
     (list (make-cartesian 10 13)
           (make-cartesian 11 13)
           (make-cartesian 3  5)
           (make-cartesian 4  5)))
   (define subtracted-pts
     (list (make-cartesian -10 -7)
           (make-cartesian   9  7)
           (make-cartesian   3 -1)
           (make-cartesian  -2  1)))
   (for-each
    (lambda (p1 p2 add sub)
      (check-point (cartesian+ p1 p2)
                   add
                   0)
      (check-point (cartesian- p1 p2)
                   sub
                   0))
    pts1
    pts2
    added-pts
    subtracted-pts))
  
  (test-case
   "cartesian-distance"
   (check-pred zero? (cartesian-distance (make-cartesian 3 3) (make-cartesian 3 3)))
   (check-= (cartesian-distance (make-cartesian 3 0) (make-cartesian 0 0)) 3 e)
   (check-= (cartesian-distance (make-cartesian 3 0) (make-cartesian 3 3)) 3 e)
   (check-= (cartesian-distance (make-cartesian 0 0) (make-cartesian 4 3)) 5 e))

  (test-case
   "cartesian-dot"
   (map
    (lambda (x1 y1 x2 y2)
      (check-= (cartesian-dot (make-cartesian x1 y1)
                              (make-cartesian x2 y2))
               (+ (* x1 x2) (* y1 y2))
               e))
    '(0 1 2 3 4 5)
    '(0 3 0 4 5 1)
    '(0 4 7 0 3 4)
    '(0 4 8 3 0 1)))

  (test-case
   "cartesian-transform"
   (check-point (cartesian-transform (make-cartesian 0 0) 1 1 0)
                (make-cartesian 1 1)
                0)
   (check-point (cartesian-transform (make-cartesian 1 1) 2 1 pi)
                (make-cartesian 1 0)
                e))

  (test-case
   "polar+ and polar-"
   (define pts1
     (list (make-polar 0 3) (make-polar 10 10) (make-polar 3 2) (make-polar 1 3)))
   (define pts2
     (list (make-polar 10 10) (make-polar 1 3) (make-polar 0 3) (make-polar 3 2)))
   (for-each
    (lambda (p1 p2)
      (check-point (polar+ p1 p2)
                   (cartesian->polar
                    (cartesian+ (polar->cartesian p1) (polar->cartesian p2)))
                   0)
      (check-point (polar- p1 p2)
                   (cartesian->polar
                     (cartesian- (polar->cartesian p1) (polar->cartesian p2)))
                   0))
    pts1
    pts2))
  )