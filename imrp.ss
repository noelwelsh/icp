#lang scheme/base

(require scheme/match
         scheme/foreign
         (planet schematics/numeric:1/vector)
         "angle.ss"
         "point.ss"
         "icp-base.ss"
         "c/base.ss")


;; Polar Polar Polar Polar -> Number
(define-icp (closest-point
             "imrp_closest_point"
             _polar _polar _polar _polar-pointer -> _double))

;; Polar Polar Number Polar -> Void
(define-icp (interpolate-point-to-angle
             "imrp_interpolate_point_to_angle"
             _polar _polar _double _polar-pointer -> _void))


(define-icp (interpolate-point-to-range
             "imrp_interpolate_point_to_range"
             _polar _polar _double _polar-pointer -> _void))


;; (Vectorof Polar) (Vectorof Polar) Number Number Number Number
;;   ->
;; (values Number Number Number)
;;
;; One iteration of the IMRP algorithm. We assume ref-pts
;; have already been projected to the same reference frame.
;;
;; xt, yt, and a are the starting transformation
;; (translation and rotation) Result is new found
;; translation and rotation, which should be applied in
;; addition to xt, yt, and a.
(define (imrp ref-pts new-pts xt yt a rotation)
  (define transformed-pts
    (vector-map
     (lambda (pt)
       (polar-normalise (cartesian->polar (cartesian-transform (polar->cartesian pt) xt yt a))))
     new-pts))
  (define matching-pts
    (imrp-matching-points-internal transformed-pts ref-pts rotation))
  (optimal-transformation transformed-pts matching-pts))


(define-icp (imrp-matching-points-internal
             "imrp_matching_points"
             (new-pts : (_vector i _polar)) (_vector i _polar)
             (n : _int = (vector-length new-pts))
             _double*
             (out : (_vector o _polar n))
             -> _void
             -> out))

(define (imrp-matching-points new-pts ref-pts rotation)
  (define matching-pts (imrp-matching-points-internal new-pts ref-pts rotation))
  (vector-map
   (lambda (pt)
     (if (and (= (polar-r pt) -1.0) (= (polar-a pt) -1.0))
         #f
         pt))
   matching-pts))


(define (imrp-matching-point pt pts rotation)
  (define match-pt
    (matching-point pt pts rotation
                    interpolate-point-to-angle closest-point))
  (if (and (= (polar-r match-pt) -1.0) (= (polar-a match-pt) -1.0))
      #f
      match-pt))


(provide
 closest-point
 interpolate-point-to-range
 interpolate-point-to-angle

 optimal-transformation

 imrp

 (rename-out [imrp-matching-points matching-points]
             [imrp-matching-point  matching-point]))
