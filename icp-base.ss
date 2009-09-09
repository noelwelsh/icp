#lang scheme/base

;; Basic API for ICP algorithms (ICP and IMRP)

(require
 scheme/match
 (planet schematics/numeric:1/vector)
 (planet schematics/numeric:1/for)
 (planet williams/science:3/statistics)
 "point.ss"
 "angle.ss"
 "util.ss")

;; (Vectorof Polar) (Vectorof Polar) Number
;; (Polar Polar Number -> Polar) (Polar Polar Polar -> (values Polar Number))
;;  ->
;; (Vectorof (U Polar #f))
(define (matching-points scan-pts model-pts rotation
                         interpolate-point-to-angle
                         closest-point)
  (for/vector ([i (vector-length scan-pts)]
               [pt (in-vector scan-pts)])
              (matching-point pt model-pts rotation
                              interpolate-point-to-angle closest-point)))

;; Polar (Vectorof Polar) Number
;; (Polar Polar Number -> Polar) (Polar Polar Polar -> (values Polar Number))
;;  ->
;; (U Polar #f)
;;
;; pts must be sorted by angle from low to high
(define (matching-point p pts rotation
                        interpolate-point-to-angle
                        closest-point)
  (match-define (polar (r a)) p)
  (define low (angle-normalise (- a rotation)))
  (define high (angle-normalise (+ a rotation)))

  (define-values (found _)
    (for/fold ([found #f] [found-dist -inf.0])
        ([p1 (in-vector pts)]
         [p2 (in-vector pts 1)])
      (match-define (polar (r1 a1)) p1)
      (match-define (polar (r2 a2)) p2)

      ;;(printf "low ~a  high ~a  a1 ~a  a2 ~a\n" low high a1 a2)
      (if (or
           (and (angle<? a1 low) (angle<? a2 low))
           (and (angle<? high a1) (angle<? high a2)))
          ;; Current points don't overlap allowable range
          (values found found-dist)
          (let*-values (([low-p]
                         (if (angle<? a1 low)
                             (interpolate-point-to-angle p1 p2 low)
                             p1))
                        ([high-p]
                         (if (angle<? high a2)
                             (interpolate-point-to-angle p1 p2 high)
                             p2))
                        ([closest closest-dist] (closest-point p low-p high-p)))
            (cond
             ;; We haven't previously found a point, so go with the one we just found
             [(not found) (values closest closest-dist)]
             [else
              (if (<= closest-dist found-dist)
                  (values closest closest-dist)
                  (values found found-dist))])))))
  found)

;; (Vectorof Polar) (Vectorof (U Polar #f)) -> (values Number Number Number)
(define (optimal-transformation ref-pts match-pts)
  (define n-actual-matches (for/sum ([x (in-vector match-pts)]) (if x 1 0)))

  (if (or (zero? n-actual-matches) (= 1 n-actual-matches))
      (begin
        (display "icp-base: Zero or one matching points. Returning no transformation.\n")
        (values 0 0 0))
      (let ()
        (define-values (scan-pts matching-pts)
          (for/vector ([i n-actual-matches 2]
                       [p (in-vector ref-pts)]
                       [m (in-vector match-pts)]
                       #:when m)
                      (values p m)))

        (define s-pts (vector-map polar->cartesian scan-pts))
        (define m-pts (vector-map polar->cartesian matching-pts))

        (define s-xs (vector-map cartesian-x s-pts))
        (define m-xs (vector-map cartesian-x m-pts))
        (define s-ys (vector-map cartesian-y s-pts))
        (define m-ys (vector-map cartesian-y m-pts))
  
        (define s-x-mean (mean s-xs))
        (define m-x-mean (mean m-xs))
        (define s-y-mean (mean s-ys))
        (define m-y-mean (mean m-ys))

        (define Sxx (sse s-xs m-xs))
        (define Syy (sse s-ys m-ys))
        (define Sxy (sse s-xs m-ys))
        (define Syx (sse s-ys m-xs))

        (if (and (zero? Sxx) (zero? Syy))
            (begin
              (display "icp-base: Sxx and Sxy both zero. Returning no transformation.\n")
              (values 0 0 0))
            (let* ([angle (atan (/ (- Sxy Syx) (+ Sxx Syy)))]
                   [t-x
                    (- m-x-mean (* s-x-mean (cos angle)) (* s-y-mean (sin angle)))]
                   [t-y
                    (- m-y-mean (* s-x-mean (sin angle)) (* s-y-mean (cos angle)))])
              ;;(printf "~a matchs: ~a ~a ~a ~a\n" n-actual-matches Sxx Syy Sxy Syx)
              (values t-x t-y angle))))))


(provide
 matching-points
 matching-point
 optimal-transformation)
