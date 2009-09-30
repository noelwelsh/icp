#lang scheme/base

(require
 (planet schematics/schemeunit:3/test)
 (planet schematics/numeric:1/vector)
 "icp-base.ss"
 "point.ss"
 "geometry.ss")

(define points
  (vector-map cartesian->polar (make-ellipse-points 0 0 100 50 0.0 0.1)))

(define (interpolate-point-to-angle pt1 pt2 a out)
  (set-polar-r! out (polar-r pt1))
  (set-polar-a! out (polar-a pt1)))

(define (closest-point pt1 pt2 a out)
  (set-polar-r! out (polar-r pt1))
  (set-polar-a! out (polar-a pt1))
  0.001)

(define/provide-test-suite icp-base-tests
  (test-case
   "matching-point"
   (check-point
    (matching-point (make-polar 100 0)
                    points
                    0.2
                    interpolate-point-to-angle
                    closest-point)
    (make-polar 100 0)
    0.001))

  (test-case
   "matching-points"
   (define matches
     (matching-points points points 0.2 interpolate-point-to-angle closest-point))
   (for ([pt1 (in-vector points)]
         [pt2 (in-vector matches)])
        (check-point pt1 pt2 0.001)))
  )