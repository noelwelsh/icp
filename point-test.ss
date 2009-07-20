#lang scheme/base

(require (planet schematics/schemeunit:3/test))
(require "point.ss")

(define/provide-test-suite point-tests
  (test-case
   "cartesian->polar and polar->cartesian are inverses"
   (define points
     (list (make-point 0 0) 
           (make-point 10 10) (make-point 3 0) (make-point 0 3)
           (make-point -1 -1) (make-point -3 0) (make-point 0 -3)
           (make-point 100 1) (make-point 1 100)))
   (for-each
    (lambda (p)
      (define p-prime (polar->cartesian (cartesian->polar p)))
      (check-= (point-x p-prime) (point-x p) 0.00001
               (format "x coordinate of point ~a/~a" p-prime p))
      (check-= (point-y p-prime) (point-y p) 0.00001
               (format "y coordinate of point ~a/~a" p-prime p)))
    points))
  )