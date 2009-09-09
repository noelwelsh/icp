#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/vector)
         "icp.ss"
         "point.ss"
         "geometry.ss")

(define e 0.001)

(define (closest-point/wrap pt pt1 pt2)
  (define out (make-polar 0 0))
  (define dist (closest-point pt pt1 pt2 out))
  (values out dist))

(define (interpolate-point-to-angle/wrap pt1 pt2 a)
  (define out (make-polar 0 0))
  (interpolate-point-to-angle pt1 pt2 a out)
  out)

(define/provide-test-suite icp-tests
  (test-case
   "matching-point finds match when end point is closest"
   (check-point
    (matching-point (make-polar 1.5 0)
                    (vector-map
                     polar-normalise
                     (vector (make-polar 3 .1) (make-polar 2 0) (make-polar 3 -.1)))
                   .2)
    (make-polar 2 0)
    e))

  (test-case
   "matching-point finds interpolated point"
   (check-point
    (matching-point (make-polar 3 0)
                    (vector-map
                     polar-normalise
                     (vector (make-polar 3 -.1)  (make-polar 2 0) (make-polar 4 .1)))
                   .2)
    (let-values (([p d]
                  (closest-point/wrap
                   (make-polar 3 0) (make-polar 2 0) (make-polar 4 .1))))
      p)
    e))

  (test-case
   "matching-point finds no match when all points outside of rotation"
   (check-false
    (matching-point (make-polar 3 0)
                    (vector (make-polar 4 2) (make-polar 3 1))
                    .1)))

  (test-case
   "matching-points finds exact matches for duplicated points"
   (define pts
     (vector
      (make-polar 12.659980129625007 6.169738940534455)
      (make-polar 12.659984467529965 6.1871918601262275)
      (make-polar 12.760034866316001 6.204645553498888)
      (make-polar 15.460035923506128 6.222098952775416)
      (make-polar 18.010041482523718 6.23955218569482)))
   (vector-map
    (lambda (pt1 pt2) (check-point pt1 pt2 e))
    (matching-points pts pts e)
    pts))
  
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
   (define ref-pts (vector-map cartesian->polar (make-ellipse-points 3 3 200 200 .10 .1)))
   (define new-pts (vector-map cartesian->polar (make-ellipse-points 6 6 200 200 .15 .1)))
   (define-values (true-xt true-yt true-a) (values -3 -3 0.05))
   (define-values (xt yt a rotation)
     (for/fold ([xt 0] [yt 0] [a 0] [rotation .1])
         ([i (in-range 100)])
       (define-values (next-xt next-yt next-a next-rotation)
         (let-values (([nxt nyt na] (icp ref-pts new-pts xt yt a rotation)))
           (values (+ xt nxt) (+ yt nyt) (+ a na) rotation)))
       ;;(printf "ICP Iteration ~a: ~a ~a ~a ~a\n" i next-xt next-yt next-a next-rotation)
       ;;(check < (abs (- next-a true-a)) (abs (- r true-a)))
       (values next-xt next-yt next-a next-rotation)))
   ;;(printf "xt: ~a  yt: ~a  a: ~a\n" xt yt a)
   (check < (abs (/ (- true-xt xt) true-xt)) .1)
   (check < (abs (/ (- true-yt yt) true-yt)) .1)
   (check < (abs (- true-a a)) .1))
            
  )