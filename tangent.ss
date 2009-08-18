#lang scheme/base

;;; Fit tangent lines to sets of points

(require scheme/math
         (planet schematics/numeric:1/vector)
         (planet williams/science:3/statistics)
         "point.ss"
         "util.ss")

;; (Vectorof Cartesian) Number Number Number -> (Vectorof (U Polar #f))
(define (fit-tangents pts neighbourhood angle-limit error-limit)
  (define n-pts (vector-length pts))
  (define n (if (odd? neighbourhood) neighbourhood (sub1 neighbourhood)))
  (define width (/ (sub1 n) 2))

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

  (for/vector ([_ (add1 (- n-pts n))]
               [i (in-range width (- n-pts width))]
               [x (in-vector pts width (- n-pts width))])
    (let-values (([normal err]
                  (fit-tangent (vector-slice pts (- i width) (+ i width))))
                 ([pt] (cartesian->polar x)))
      (if (or (> err error-limit)
              (> (- (polar-a normal) (polar-a pt)) angle-limit))
          #f
          normal))))

;; (Vectorof Cartesian) -> (values Polar Number)
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
  (define error (/ (+ Sx Sy
                      (- (sqrt (+ (* 4 (square Sxy)) (square (- Sy Sx)))))) 2))
  ;;(printf "~a ~a ~a ~a ~a\n" x-mean y-mean Sx Sy Sxy)
  (values
   (if (< length 0)
       (make-polar (- length) (+ pi angle))
       (make-polar length angle))
   error))



(provide
 fit-tangent
 fit-tangents)