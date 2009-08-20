#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/vector)
         "icp.ss"
         "point.ss"
         "geometry.ss")

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
   "matching-points handles matches that are #f"
   (define-values (x1 y1 a1)
     ;; Some matches are #f
     (optimal-transformation
      (list->vector
       (map (lambda (a) (make-polar 3 a))
            (list 0 .1 .2 .3 .4 .5)))
      (list->vector
       (map (lambda (a) (if a (make-polar 3 (+ a .03)) #f))
            (list 0 .1 .2 #f .4 .5)))))
   (define-values (x2 y2 a2)
     ;; #f matches above are simply removed
     (optimal-transformation
      (list->vector
       (map (lambda (a) (make-polar 3 a))
            (list 0 .1 .2 .4 .5)))
      (list->vector
       (map (lambda (a) (make-polar 3 (+ a .03)))
            (list 0 .1 .2 .4 .5)))))
   (check-equal? x1 x2)
   (check-equal? y1 y2)
   (check-equal? a1 a2))

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
   (define ref-pts (vector-map cartesian->polar (make-ellipse-points 3 3 40 20 .10 .1)))
   (define new-pts (vector-map cartesian->polar (make-ellipse-points 6 6 40 20 .15 .1)))
   (define-values (true-tx true-ty true-a) (values -3 -3 -.05))
   (define-values (tx ty a rotation)
     (for/fold ([tx 0] [ty 0] [a 0] [rotation .1])
         ([i (in-range 10)])
       (define-values (next-tx next-ty next-a next-rotation)
         (let-values (([ntx nty na] (icp ref-pts new-pts tx ty a rotation)))
           (values (+ tx ntx) (+ ty nty) (+ a na) (* 2 (abs na)))))
       ;;(printf "Iteration ~a: ~a ~a ~a ~a\n" i next-tx next-ty next-a next-rotation)
       ;;(check <
       ;;       (cartesian-distance (make-cartesian next-tx next-ty) (make-cartesian true-tx true-ty))
       ;;       (cartesian-distance (make-cartesian tx ty) (make-cartesian true-tx true-ty)))
       ;;(check < (abs (- next-a true-a)) (abs (- r true-a)))
       (values next-tx next-ty next-a next-rotation)))
   (check < (abs (/ (- true-tx tx) true-tx)) .11)
   (check < (abs (/ (- true-ty ty) true-tx)) .11)
   (check < (abs (/ (- true-a a) true-a)) .11))
            
  )