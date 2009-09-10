#lang scheme/base

(require scheme/match
         scheme/foreign
         (planet schematics/numeric:1/vector)
         "point.ss"
         "geometry.ss"
         "angle.ss"
         "icp-base.ss"
         "c/base.ss")


;; Polar Polar Polar Polar -> Number
(define-icp (closest-point
             "icp_closest_point"
             _polar _polar _polar _polar-pointer -> _double))


;; Polar Polar Number Polar -> Void
(define-icp (interpolate-point-to-angle
             "icp_interpolate_point_to_angle"
             _polar _polar _double _polar-pointer -> _void))


(define-icp (interpolate-point-to-range
             "icp_interpolate_point_to_range"
             _polar _polar _double _polar-pointer -> _void))


(define-icp (icp-matching-points-internal
             "icp_matching_points"
             (new-pts : (_vector i _polar)) (_vector i _polar)
             (n : _int = (vector-length new-pts))
             _double*
             (out : (_vector o _polar n))
             -> _void
             -> out))


;; (Vectorof Polar) (Vectorof Polar) Number Number Number Number
;;   ->
;; (values Number Number Number)
;;
;; One iteration of the IDC algorithm. We assume ref-pts
;; have already been projected to the same reference frame.
;;
;; xt, yt, and a are the starting transformation
;; (translation and rotation) Result is new found
;; translation and rotation, which should be applied in
;; addition to xt, yt, and a.
(define-icp (icp
             "icp"
             (ref-pts new-pts xt yt a rotation)
             ::
             (ref-pts : (_vector i _polar))
             (new-pts : (_vector i _polar))
             (n : _int = (vector-length ref-pts))
             (xt : _double*) (yt : _double*) (a : _double*) (rotation : _double*)
             (xt_out : (_ptr o _double))
             (yt_out : (_ptr o _double))
             (a_out : (_ptr o _double))
             ->
             _void
             -> (values xt_out yt_out a_out)))

(define (icp-matching-points new-pts ref-pts rotation)
  (define matching-pts (icp-matching-points-internal new-pts ref-pts rotation))
  (vector-map
   (lambda (pt)
     (if (and (= (polar-r pt) -1.0) (= (polar-a pt) -1.0))
         #f
         pt))
   matching-pts))


(define (icp-matching-point pt pts rotation)
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

 icp

 (rename-out [icp-matching-points matching-points]
             [icp-matching-point  matching-point]))