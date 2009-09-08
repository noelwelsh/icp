#lang scheme/base

(require scheme/match
         (planet schematics/numeric:1/vector)
         "point.ss"
         "geometry.ss"
         "angle.ss"
         "icp-base.ss")


;; Polar Polar Polar -> (values Polar Number)
(define (closest-point p p1 p2)
  (define-values (closest-pt dist)
    (line-segment-closest-point (polar->cartesian p1) (polar->cartesian p2)
                                (polar->cartesian p)))
  (values (polar-normalise (cartesian->polar closest-pt)) dist))

;; Polar Polar Number -> Polar
(define (interpolate-point-to-angle p1 p2 a)
  (polar-normalise
   (cartesian->polar
    (line-line-intersection
     (polar->cartesian p1) (polar->cartesian p2)
     (polar->cartesian (make-polar 1 a)) (make-cartesian 0 0)))))


(define (interpolate-point-to-range p1 p2 r)
  (match-define (struct polar (r1 a1)) p1)
  (match-define (struct polar (r2 a2)) p2)

  (define a
    (angle-normalise
     (* (/ (- r1 r2))
        (+ (/ (* r1 r2 (angle-normalise (- a2 a1))) r)
           (- (* r1 a1) (* r2 a2))))))
  (make-polar r a))


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
(define (icp ref-pts new-pts xt yt a rotation)
  (define transformed-pts
    (vector-map
     (lambda (pt)
       (polar-normalise (cartesian->polar (cartesian-transform (polar->cartesian pt) xt yt a))))
     new-pts))
  (define matching-pts
    (matching-points transformed-pts ref-pts rotation
                     interpolate-point-to-angle
                     closest-point))
  ;;(printf "ICP ~a ~a ~a ~a\n" xt yt a rotation)
  (optimal-transformation transformed-pts matching-pts))



;; These are defined for tests
(define (icp-matching-points new-pts ref-pts rotation)
  (matching-points new-pts ref-pts rotation
                   interpolate-point-to-angle
                   closest-point))

(define (icp-matching-point pt pts rotation)
  (matching-point pt pts rotation
                  interpolate-point-to-angle closest-point))

(provide
 closest-point
 interpolate-point-to-range
 interpolate-point-to-angle

 optimal-transformation

 icp

 (rename-out [icp-matching-points matching-points]
             [icp-matching-point  matching-point]))