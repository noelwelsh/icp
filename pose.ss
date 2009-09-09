#lang scheme/base

(require
 scheme/match
 scheme/foreign
 (planet schematics/numeric:1/vector)
 "point.ss"
 "c/base.ss")


;; struct pose : Number Number Number
;;
;; Pose in 2-D: x, y, and angle
(define-cstruct _pose
  ([x _double*]
   [y _double*]
   [a _double*]))

;; Pose (Vectorof Polar) -> (Vectorof Polar)
(define (pose-transform-points pose points)
  (define a (pose-a pose))
  (define p (make-cartesian (pose-x pose) (pose-y pose)))
  (vector-map
   (lambda (pt)
     (cartesian->polar (cartesian+ (polar->cartesian (polar-rotate pt a)) p)))
   points))

(define (pose->vector p)
  (vector (pose-x p) (pose-y p) (pose-a p)))

(define-match-expander pose
  (syntax-rules ()
    [(pose (x y a))
     (app pose->vector (vector x y a))]))

(provide
 pose
 make-pose
 pose?
 pose-x
 pose-y
 pose-a
 pose->vector
 
 pose-transform-points)