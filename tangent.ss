#lang scheme/base

;;; Fit tangent lines to sets of points

(require scheme/math
         (planet schematics/numeric:1/vector)
         (planet williams/science:3/statistics)
         "point.ss"
         "util.ss")

(define (square x) (* x x))

;; (Vectorof Cartesian) Number -> (Vectorof Polar)
(define (fit-tangents pts neighbourhood)
  (define n-pts (vector-length pts))
  (define n (if (odd? neighbourhood) neighbourhood (sub1 neighbourhood)))
  (define width (/ n 2))

  (when (> neighbourhood n-pts)
    (raise-mismatch-error
     'fit-tangents
     (format "Given ~a points, which is less than the neighbourhood size of " n-pts)
     neighbourhood))
  (when (< neighbourhood 3)
    (raise-mismatch-error
     'fit-tangents
     "The minimum size neighbourhood is 3; given neighbourhood size:")
     neighbourhood)

  ;; TODO
  ;; Compute incidence angle
  ;; Discard tangents where incidence angle or error too high
  
  (for/vector ([_ (add1 (- n-pts n))]
               [i (in-range width (- n-pts width))])
              (fit-tangent (vector-slice pts (- i width) (+ i width)))))

;; (Vectorof Cartesian) -> Polar
(define (fit-tangent pts)
  (define xs (vector-map cartesian-x pts))
  (define ys (vector-map cartesian-y pts))

  (define x-mean (mean xs))
  (define y-mean (mean ys))
  
  (define Sx (sse xs))
  (define Sy (sse ys))
  (define Sxy (sse xs ys))

  (define angle (/ (atan (* -2 Sxy) (- Sy Sx)) 2))
  (define length (+ (* x-mean (cos angle)) (* y-mean (sin angle))))
  (define error (/ (+ Sx Sy (- (sqrt (+ (* 4 Sxy) (square (- Sx Sy)))))) 2))
  ;;(printf "~a ~a ~a ~a ~a\n" x-mean y-mean Sx Sy Sxy)
  (if (< length 0)
      (make-polar (- length) (+ pi angle))
      (make-polar length angle)))



(provide
 fit-tangent
 fit-tangents)