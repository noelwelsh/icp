#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/vector)
         "icp.ss"
         "point.ss")

(define e 0.001)

;; Number Number Number Number Number Number -> (Vectorof Cartesian)
;;
;; Constructs points evenly spaced a step radians around an ellipse.
;; xc and yc are the center coordinates
;; a and b are the semimajor and semiminor axis respectively
;; phi is the angle the semimajor axis makes to the x axis
(define (make-ellipse-points xc yc a b phi step)
  (list->vector
   (for/list ([t (in-range 0 (* 2 pi) step)])
     (make-cartesian (+ xc
                        (* a (cos t) (cos phi))
                        (- (* b (sin t) (sin phi))))
                     (+ yc
                        (* a (cos t) (sin phi))
                        (* b (sin t) (cos phi)))))))

(define/provide-test-suite icp-tests
  (test-case
   "matching-point finds match when end point is closest"
   (check-point
    (matching-point (make-polar 1.5 0)
                    (vector (make-polar 3 .1) (make-polar 2 0) (make-polar 3 -.1))
                   .2)
    (make-polar 2 0)
    e))

  (test-case
   "matching-point finds interpolated point"
   (check-point
    (matching-point (make-polar 3 0)
                    (vector (make-polar 3 -.1)  (make-polar 2 0) (make-polar 4 .1))
                   .2)
    (let-values (([p d]
                  (closest-point (make-polar 3 0) (make-polar 2 0) (make-polar 4 .1))))
      p)
    e))

  (test-case
   "matching-point find no match when all points outside of rotation"
   (check-false
    (matching-point (make-polar 3 0)
                    (vector (make-polar 4 2) (make-polar 3 1))
                    .1)))

  (test-case
   "icp iteration moves rotation closer to optimal"
   ;; Points on an arc
   (define ref-pts (list->vector
                    (map (lambda (a) (make-polar 3 a))
                         (list 0 .1 .2 .3 .4 .5))))
   ;; Same points rotated by a constant amount
   (define new-pts (list->vector
                    (map (lambda (a) (make-polar 3 (+ a .03)))
                         (list 0 .1 .2 .3 .4 .5))))
   (define-values (xt yt a) (icp ref-pts new-pts 0 0 0 .25))
   (check < a 0))

  (test-case
   "Iterations of icp converge to true transform for an ellipse"
   (define ref-pts (vector-map cartesian->polar (make-ellipse-points 5 5 3 2 .1 .1)))
   (define new-pts (vector-map cartesian->polar (make-ellipse-points 6 6 3 2 .15 .1)))
   (fail "Not implemented"))
  )