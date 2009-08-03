#lang scheme/base

(require scheme/math
         scheme/match
         (planet schematics/numeric:1/vector))

;; struct polar : number number
(define-struct polar (r a) #:transparent)
;; struct cartesian : number number
(define-struct cartesian (x y) #:transparent)


(define (polar->cartesian p)
  (match-define (struct polar (r a)) p)
  (define x (* r (cos a)))
  (define y (* r (sin a)))

  (make-cartesian x y))

(define (cartesian->polar p)
  (match-define (struct cartesian (x y)) p)
  (define r (sqrt (+ (* x x) (* y y))))
  (define a (if (and (zero? y) (zero? x))
                0
                (atan y x)))

  (make-polar r a))

(define (cartesian+ p1 p2)
  (match-define (struct cartesian (x1 y1)) p1)
  (match-define (struct cartesian (x2 y2)) p2)

  (make-cartesian (+ x1 x2) (+ y1 y2)))

(define (cartesian- p1 p2)
  (match-define (struct cartesian (x1 y1)) p1)
  (match-define (struct cartesian (x2 y2)) p2)

  (make-cartesian (- x1 x2) (- y1 y2)))



(define (polar+ p1 p2)
  ;; Note that r can also be calculated as
  ;; r^2 = r1^2 + r2^2 + 2r1r2 cos(a1 - a2)
  (match-define (struct polar (r1 a1)) p1)
  (match-define (struct polar (r2 a2)) p2)

  (cartesian->polar (cartesian+ (polar->cartesian p1) (polar->cartesian p2))))

(define (polar- p1 p2)
  (match-define (struct polar (r1 a1)) p1)
  (match-define (struct polar (r2 a2)) p2)

  (cartesian->polar (cartesian- (polar->cartesian p1) (polar->cartesian p2))))

(define (polar-dot p1 p2)
  (match-define (struct polar (r1 a1)) p1)
  (match-define (struct polar (r2 a2)) p2)

  (* r1 r2 (cos (- a1 a2))))

(define (polar-rotate p theta)
  (match-define (struct polar (r a)) p)

  (make-polar r (+ a theta)))


(provide
 (struct-out polar)
 (struct-out cartesian)
 
 polar->cartesian
 cartesian->polar

 cartesian+
 cartesian-

 polar+
 polar-
 polar-dot
 polar-rotate)
