#lang scheme/base

(require (planet schematics/numeric:1/vector)
         "point.ss")


;; struct pose : Number Number Number
;;
;; Pose in 2-D: x, y, and angle
(define-struct pose (x y a) #:transparent)

;; Pose (Vectorof Polar) -> (Vectorof Polar)
(define (pose-transform-points pose points)
  (define a (pose-a pose))
  (define p (make-cartesian (pose-x pose) (pose-y pose)))
  (vector-map
   (lambda (pt)
     (cartesian->polar (cartesian+ (polar->cartesian (polar-rotate pt a)) p)))
   points))

(provide
 (struct-out pose)

 pose-transform-points)