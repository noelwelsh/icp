#lang scheme/base

;; Basic API for ICP algorithms (ICP and IMRP)

(require
 scheme/match
 scheme/foreign
 (planet williams/science:3/statistics)
 (planet schematics/numeric:1/for)
 (planet schematics/numeric:1/vector)
 "point.ss"
 "angle.ss"
 "util.ss"
 "c/base.ss")

(define _interpolate_point_to_angle
  (_fun _polar _polar _double _polar-pointer -> _void))

(define _closest_point
  (_fun _polar _polar _polar _polar-pointer -> _double))

;; (Vectorof Polar) (Vectorof Polar) Number
;; (Polar Polar Number -> Polar) (Polar Polar Polar -> (values Polar Number))
;;  ->
;; (Vectorof (U Polar #f))
(define-icp (matching-points
             "matching_points"
             (new-pts : (_vector i _polar)) (n : _int = (vector-length new-pts))
             (ref-pts : (_vector i _polar)) (_int = n)
             _double*
             _interpolate_point_to_angle
             _closest_point
             (out : (_vector o _polar n))
             -> _void
             -> out))

;; Polar (Vectorof Polar) Number
;; (Polar Polar Number -> Polar) (Polar Polar Polar -> (values Polar Number))
;;  ->
;; (U Polar #f)
;;
;; pts must be sorted by angle from low to high
(define-icp (matching-point
             "matching_point"
             _polar (pts : (_vector i _polar)) (_int = (vector-length pts))
             _double*
             _interpolate_point_to_angle
             _closest_point
             (out : (_ptr o _polar))
             -> _void
             -> out))

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
 _interpolate_point_to_angle
 _closest_point
 
 matching-points
 matching-point
 optimal-transformation)
