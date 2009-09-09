#lang scheme/base

(require scheme/match
         (planet schematics/numeric:1/vector)
         "angle.ss"
         "point.ss"
         "icp-base.ss")


;; Polar Polar Polar Polar -> Number
(define (closest-point p p1 p2 out)
  (match-define (polar (r a)) p)
  (match-define (polar (r1 a1)) p1)
  (match-define (polar (r2 a2)) p2)

  (define closest-pt
    (cond
     [(and (<= r1 r) (<= r2 r))
      (if (> r1 r2)
          p1
          p2)]
     [(and (> r1 r) (> r2 r))
      (if (< r1 r2)
          p1
          p2)]
     [else
      (interpolate-point-to-range p1 p2 r)]))
  (set-polar-r! out (polar-r closest-pt))
  (set-polar-a! out (polar-a closest-pt))

  (cartesian-distance (polar->cartesian p) (polar->cartesian closest-pt)))

(define (interpolate-point-to-angle p1 p2 a out)
  (match-define (polar (r1 a1)) p1)
  (match-define (polar (r2 a2)) p2)

  (define r
    (/ (* r1 r2 (angle-normalise (- a2 a1)))
       (+ (* r1 (angle-normalise (- a a1))) (* r2 (angle-normalise (- a2 a))))))
  (set-polar-r! out r)
  (set-polar-a! out a))

(define (interpolate-point-to-range p1 p2 r)
  (match-define (polar (r1 a1)) p1)
  (match-define (polar (r2 a2)) p2)

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
    (matching-points transformed-pts ref-pts rotation
                     interpolate-point-to-angle
                     closest-point))
  (optimal-transformation transformed-pts matching-pts))



;; These are defined for tests
(define (imrp-matching-points new-pts ref-pts rotation)
  (matching-points new-pts ref-pts rotation
                   interpolate-point-to-angle
                   closest-point))

(define (imrp-matching-point pt pts rotation)
  (matching-point pt pts rotation
                  interpolate-point-to-angle closest-point))


(provide
 closest-point
 interpolate-point-to-range
 interpolate-point-to-angle

 optimal-transformation

 imrp

 (rename-out [imrp-matching-points matching-points]
             [imrp-matching-point  matching-point]))
