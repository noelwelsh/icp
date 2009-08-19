#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "geometry.ss"
         "point.ss"
         "pose.ss")

(define e 0.00001)

(define-check (check-closest-point pt1 pt2 pt expected-pt dist)
  (define-values (closest-pt closest-dist) (line-segment-closest-point pt1 pt2 pt))
  (check-point closest-pt expected-pt e)
  (check-= closest-dist dist e))

(define/provide-test-suite geometry-tests
  (test-case
   "line-segment-closest-point, coincident pt1 and pt2"
   (check-closest-point (make-cartesian 0 0) (make-cartesian 0 0)
                        (make-cartesian 0 4)
                        (make-cartesian 0 0) 4))

  (test-case
   "line-segment-closest-point, end points are closest"
   (check-closest-point (make-cartesian 2 1) (make-cartesian 3 4)
                        (make-cartesian 0 0)
                        (make-cartesian 2 1) (sqrt 5))
   (check-closest-point (make-cartesian -2 1) (make-cartesian -1 1)
                        (make-cartesian 0 0)
                        (make-cartesian -1 1) (sqrt 2)))

  (test-case
   "line-segment-closest-point, normal case"
   (check-closest-point (make-cartesian -3 3) (make-cartesian 3 3)
                        (make-cartesian 0 0)
                        (make-cartesian 0 3) 3))

  (test-case
   "line-segment-closest-point, point intersects segment"
   (check-closest-point (make-cartesian -3 3) (make-cartesian 3 3)
                        (make-cartesian 0 3)
                        (make-cartesian 0 3) 0))


  (test-case
   "line-line-intersection, horizontal and vertical line"
   (check-point (line-line-intersection
                 (make-cartesian -3 0) (make-cartesian 3 0)
                 (make-cartesian 0 -3) (make-cartesian 0 3))
                (make-cartesian 0 0)
                e))

  (test-case
   "line-line-intersection, diagonal lines"
   (check-point (line-line-intersection
                 (make-cartesian -3 -3) (make-cartesian 3 3)
                 (make-cartesian -3 3) (make-cartesian 3 -3))
                (make-cartesian 0 0)
                e))

    (test-case
   "project-point"
   ;; Point is at (0, 1) in global Cartesian coordinates
   (define pt
     (project-point (make-polar 1 0)
                    (make-pose 0 0 (/ pi 2))
                    (make-pose 1 0 (* pi 3/2))))

   ;; Point is (2, 2) in global Cartesian coordinates
  (define pt2
    (project-point (make-polar 2 (* pi 7/4))
                   (make-pose 2 0 (* 3/4 pi))
                   (make-pose 4 2 (* pi 7/4))))

   (check-= (polar-r pt) (sqrt 2) e)
   (check-= (polar-a pt) (* pi 5/4) e)

   (check-= (polar-r pt2) 2 e)
   (check-= (polar-a pt2) (* pi 5/4) e))

  (test-case
   "project-points"
   (define pts (vector (make-polar 1 0) (make-polar 2 .3) (make-polar 0 .1)))
   (define ref-pose (make-pose 4 2 2))
   (define new-pose (make-pose 3 7 3))
   (define proj-pts (project-points pts ref-pose new-pose))
   (for ([pt (in-vector pts)]
         [actual (in-vector proj-pts)])
        (check-equal? actual (project-point pt ref-pose new-pose))))
  
  )