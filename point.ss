#lang scheme/base

(require scheme/math
         scheme/match
         (planet schematics/numeric:1/vector)
         (planet schematics/schemeunit:3))

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

(define (cartesian-distance p1 p2)
  (match-define (struct cartesian (x y)) (cartesian- p1 p2))

  (sqrt (+ (* x x) (* y y))))

(define (cartesian-dot p1 p2)
  (match-define (struct cartesian (x1 y1)) p1)
  (match-define (struct cartesian (x2 y2)) p2)

  (+ (* x1 x2) (* y1 y2)))


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



(define-check (check-point p1 p2 e)
  (check-true (or (and (cartesian? p1)
                       (cartesian? p2))
                  (and (polar? p1)
                       (polar? p2)))
              (format "Points ~a and ~a are of different types" p1 p2))
  (if (cartesian? p1)
      (begin
        (check-= (cartesian-x p1) (cartesian-x p2) e
                 (format "Points ~a and ~a x coordinates differ by more than ~a"
                         p1 p2 e))
        (check-= (cartesian-y p1) (cartesian-y p2) e
                 (format "Points ~a and ~a y coordinates differ by more than ~a"
                         p1 p2 e)))
      (begin
        (check-= (polar-r p1) (polar-r p2) e
                 (format "Points ~a and ~a radii differ by more than ~a"
                         p1 p2 e))
        (check-= (polar-a p1) (polar-a p2) e
                 (format "Points ~a and ~a angles differ by more than ~a"
                         p1 p2 e)))))

(provide
 (struct-out polar)
 (struct-out cartesian)
 
 polar->cartesian
 cartesian->polar

 cartesian+
 cartesian-
 cartesian-distance
 cartesian-dot
 
 polar+
 polar-
 polar-dot
 polar-rotate

 check-point)
