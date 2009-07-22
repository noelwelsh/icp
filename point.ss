#lang scheme/base

(require scheme/math
         scheme/match
         (planet schematics/numeric:1/vector))

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


(define (point-x p)
  (vector-ref p 0))

(define (point-y p)
  (vector-ref p 1))


(define (polar->cartesian p)
  (match-define (vector r a) p)
  (define x (* r (cos a)))
  (define y (* r (sin a)))

  (make-point x y))

(define (cartesian->polar p)
  (match-define (vector x y) p)
  (define r (sqrt (+ (* x x) (* y y))))
  (define a (if (and (zero? y) (zero? x))
                0
                (atan y x)))

  (make-point r a))

(define point+ vector+)
(define point- vector-)

(provide make-point
         
         point-r
         point-a

         point-x
         point-y

         polar->cartesian
         cartesian->polar

         point+
         point-)
