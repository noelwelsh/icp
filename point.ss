#lang scheme/base

(require (planet schematics/numeric:1/vector))

;; type point : (vector number number)
;;
;; A point in polar cooardinates

;; radius and angle
(define (make-point r a)
  (vector r a))

(define (point-r p)
  (vector-ref p 0))

(define (point-a p)
  (vector-ref p 1))




(provide make-point
         point-r
         point-a)
