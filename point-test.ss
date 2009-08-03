#lang scheme/base

(require (planet schematics/schemeunit:3/test))
(require "point.ss")

(define/provide-test-suite point-tests
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
      (check-= (cartesian-x p-prime) (cartesian-x p) 0.00001
               (format "x coordinate of point ~a/~a" p-prime p))
      (check-= (cartesian-y p-prime) (cartesian-y p) 0.00001
               (format "y coordinate of point ~a/~a" p-prime p)))
    points))

  (test-case
   "polar+ and polar-"
   (define pts1
     (list (make-polar 0 3) (make-polar 10 10) (make-polar 3 2) (make-polar 1 3)))
   (define pts2
     (list (make-polar 10 10) (make-polar 1 3) (make-polar 0 3) (make-polar 3 2)))
   (for-each
    (lambda (p1 p2)
      (check-equal? (polar+ p1 p2)
                    (cartesian->polar
                     (cartesian+ (polar->cartesian p1) (polar->cartesian p2))))
      (check-equal? (polar- p1 p2)
                    (cartesian->polar
                     (cartesian- (polar->cartesian p1) (polar->cartesian p2)))))
    pts1
    pts2))
   
  )