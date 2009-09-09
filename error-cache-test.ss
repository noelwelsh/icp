#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/vector)
         (planet schematics/numeric:1/matrix)
         "error-cache.ss"
         "geometry.ss"
         "point.ss"
         "pose.ss"
         "austin-laser-points.ss")

(define austin-points
  (vector-slice points 4))

(define austin-poses
  (vector-slice poses 4))


(define/provide-test-suite error-cache-tests
  (test-case
   "create-cache doesn't fail"
   (create-cache (vector (vector-map
                          cartesian->polar
                          (make-ellipse-points 1 1 100 75 2 0.2))
                         (vector-map
                          cartesian->polar
                          (make-ellipse-points 3 4 120 60 .1 0.2)))
                 (vector (make-pose 1.5 0.5 1.9)
                         (make-pose 3 3 .1))))

  (test-case
   "cache-ref returns expected value"
   (define cache
     (make-cache (matrix 2 2  0 1 2 3) 1.5))
   (check = (cache-ref cache 0 0) 0)
   (check = (cache-ref cache 0 1) 1)
   (check = (cache-ref cache 1 0) 2)
   (check = (cache-ref cache 1 1) 3))

  (test-case
   "read-cache and write-cache are inverse"
   (after
    (let ([cache (make-cache (matrix 2 2  0 1 2 3) 1.5)])
      (write-cache cache "foo.txt")
      (let ([cache1 (read-cache "foo.txt")])
        (check = (cache-ref cache 0 0) (cache-ref cache1 0 0))
        (check = (cache-ref cache 0 1) (cache-ref cache1 0 1))
        (check = (cache-ref cache 1 0) (cache-ref cache1 1 0))
        (check = (cache-ref cache 1 1) (cache-ref cache1 1 1))
        (check = (cache-std-dev cache) (cache-std-dev cache1))))
    (delete-file "foo.txt")))

  (test-case
   "error-cache error is zero when ref and new are the same"
   (let ([cache (create-cache austin-points austin-poses)])
     (check-pred zero? (cache-ref cache 0 0))
     (check-pred zero? (cache-ref cache 1 1))
     (check-pred zero? (cache-ref cache 2 2))
     (check-pred zero? (cache-ref cache 3 3))))
   
  )