#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "tangent.ss"
         "point.ss")

(define e 0.00001)

(define (fit-tangent->normal pts)
  (let-values (([normal err] (fit-tangent pts)))
    normal))

(define/provide-test-suite tangent-tests
  (test-case
   "fit-tangent without noise"
   ;; A line at 45 degress
   (check-point
    (fit-tangent->normal (list->vector
                          (map (lambda (x) (make-cartesian x (+ x 3)))
                               '(-4 -2 0 3 5 6))))
    (make-polar (/ (sqrt 18) 2) (* 3/4 pi))
    e)
   ;; A horizontal line
   (check-point
    (fit-tangent->normal (vector (make-cartesian -1 -2) (make-cartesian 0 -2)
                                 (make-cartesian  2 -2) (make-cartesian 3 -2)))
    (make-polar 2 (* 3/2 pi))
    e)
   ;; A vertical line
   (check-point
    (fit-tangent->normal (vector (make-cartesian -2 -1) (make-cartesian -2 0)
                                 (make-cartesian -2 2) (make-cartesian -2 3)))
    (make-polar 2 pi)
    e))

  (test-case
   "fit-tangent has no error for noise-free points"
   (let-values (([normal err]
                 (fit-tangent (list->vector
                               (map (lambda (x) (make-cartesian x (+ x 3)))
                                    '(-4 -2 0 3 5 6))))))
     (check-pred zero? err)))
  
  (test-case
   "fit-tangents discards noisy tangents"
   (check-equal?
    (fit-tangents (vector (make-cartesian 2 4) (make-cartesian 40 3)
                          (make-cartesian -13 345) (make-cartesian 2 -554)
                          (make-cartesian 3 554) (make-cartesian 34 -12))
                  5 2 2)
    (vector #f #f)))
  
  (test-case
   "fit-tangents returns the correct number of points"
   (define pts (fit-tangents
                (vector (make-cartesian 0 3) (make-cartesian -3 3)
                        (make-cartesian -2 3) (make-cartesian -1 3)
                        (make-cartesian 0 3) (make-cartesian 1 3)
                        (make-cartesian 2 3) (make-cartesian 3 3))
                3 0 0))
   (check-equal? (vector-length pts) 6))
  )