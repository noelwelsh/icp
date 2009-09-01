#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/matrix)
         (planet williams/science:3/random-distributions/gaussian)
         "probability-model.ss"
         "error-cache.ss")


(define (display-cluster c)
  (display-matrix (cache-errors (cluster-cache c)))(newline))

(define/provide-test-suite probability-model-tests
  (test-case
   "Same data becomes more probable as average distance decreases"
   (let* ([c (create-cluster
              (make-cache
               (matrix 3 3  0 5 10
                            5 0 15
                            10 15 0)
               5))]
          [c1 (cluster-add c 1)]
          [c2 (cluster-add c1 0)])
     (check < (cluster-likelihood c1 2) (cluster-likelihood c2 2))))

  (test-case
   "Cache's average distance used for empty cluster"
   (define c (create-cluster
              (make-cache
               (matrix 3 3  0 5 10
                            5 0 15
                            10 15 0)
               5)))
   (check =
          (cluster-likelihood c 2)
          (gaussian-pdf 5 0 5)))
  
  (test-case
   "cluster-add and cluster-remove are inverses"
   (let* ([cache (make-cache
                  (matrix 3 3  0 5 10
                               5 0 15
                               10 15 0)
                  5)]
          [c (create-cluster cache)]
          [c1 (cluster-add c 2)]
          [c2 (cluster-remove c1 2)])
     (check = (cluster-likelihood c 1) (cluster-likelihood c2 1))))

  )