#lang scheme/base

(require scheme/match)

;; Point (Vectorof Point) Number -> Point
;;
;; pts must be sorted by angle from low to high
(define (matching-point p pts rotation)
  )

;; Point Point Point Number -> Point
(define (closest-point p p1 p2)
  (match-define (vector r a) p)
  (match-define (vector r1 a1) p1)
  (match-define (vector r2 a2) p2)

  (cond
   [(and (< r1 r) (< r2 r))
    (if (> r1 r2)
        p1
        p2)]
   [(and (> r1 r) (> r2 r))
    (if (< r1 r2)
        p1
        p2)]
   [else
    (interpolate-point-to-range p1 p2 r)]))

(define (interpolate-point-to-angle p1 p2 a)
  (match-define (vector r1 a1) p1)
  (match-define (vector r2 a2) p2)

  (define r
    (/ (* r1 r2 (- a2 a1))
       (+ (* r1 (- a a1)) (* r2 (- a2 a)))))
  (make-point r a))

(define (interpolate-point-to-range p1 p2 r)
  (match-define (vector r1 a1) p1)
  (match-define (vector r2 a2) p2)

  (define a
    (* (/ (- r1 r2))
       (+ (/ (* r1 r2 (- a2 a1)) r)
          (- (* r1 a1) (* r2 a2)))))
  (make-point r a))