#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "icp.ss"
         "point.ss")

(define e 0.001)

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
   "icp finds rotation close to optimal"
   ;; Points on an arc
   (define ref-pts (list->vector
                    (map (lambda (a) (make-polar 3 a))
                         (list 0 .1 .2 .3 .4 .5))))
   ;; Same points rotated by a constant amount
   (define new-pts (list->vector
                    (map (lambda (a) (make-polar 3 (+ a .02)))
                         (list 0 .1 .2 .3 .4 .5))))
   (define-values (xt yt a) (icp ref-pts new-pts 0 0 0 .25))
   (check-= a -.02 e)
   (check-= xt 0.0 e)
   (check-= yt 0.0 e))
  )