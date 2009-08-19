#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "slsma.ss"
         "point.ss"
         "pose.ss")

(define e 0.00001)

(define/provide-test-suite slsma-tests
  (test-case
   "filter-bounded-obstacle"
   (check-equal? (filter-bounded-obstacle
                  (vector (make-polar 10  0) (make-polar 10 .1) (make-polar 10 .2)
                          (make-polar 10 .1) (make-polar 10  0) (make-polar 10 .1)))
                 (vector (make-polar  10  0) (make-polar 10 .1) (make-polar 10 .1))))

  (test-case
   "filter-opaque removes points obscured by others in same set"
   (check-equal? (filter-opaque
                  (vector (make-polar 10 0) (make-polar 5 .5) (make-polar 10 .7)
                          (make-polar 10 2) (make-polar 5 .5) (make-polar 5 3))
                  (vector)
                  1)
                 (vector (make-polar 5 .5) (make-polar 10 2) (make-polar 5 .5) (make-polar 5 3))))

  (test-case
   "filter-opaque removes points obscured by new-pts"
   (check-equal? (filter-opaque
                  (vector (make-polar 10 0) (make-polar 5 2) (make-polar 10 3))
                  (vector (make-polar 5 1.5) (make-polar 5 2.5))
                  1)
                 (vector (make-polar 10 0) (make-polar 5 2))))

  (test-case
   "matching-point finds exact match"
   (define-values (pt normal)
     (matching-point (make-polar 4 0)
                     (make-polar 4 0)
                     (list->vector
                      (map make-polar (list 4 4 4 4 4 4) (list 6 3 0 3 6 9)))
                     (list->vector
                      (map make-polar (list 4 4 4 4 4 4) (list 6 3 0 3 6 9)))
                     0
                     10
                     10))
    (check-equal? pt (make-polar 4 0))
    (check-equal? normal (make-polar 4 0)))

  (test-case
   "matching-points find expected matches"
   (define pts (vector (make-polar 4 0) (make-polar 4 (/ pi 2))
                       (make-polar 4 pi) (make-polar 4 (* pi 3/2))))
   (define-values (matches normals)
     (matching-points pts pts pts pts 0 10 10))
   (check-equal? matches pts)
   (check-equal? normals pts))

  (test-case
   "optimise-translation finds optimal"
   ;; A square. The normals are the same as the points
   (define pts (vector (make-polar 4 0) (make-polar 4 (/ pi 2))
                      (make-polar 4 pi) (make-polar 4 (* pi 3/2))))
   ;; Square translated and rotated
   (define t-pts
     (list->vector
      (map (lambda (pt)
             (cartesian->polar
              (cartesian+ (polar->cartesian (polar-rotate pt (/ pi 4)))
                          (make-cartesian .2 .2))))
           (vector->list pts))))
   (define-values (t err) (optimise-translation pts pts t-pts t-pts (/ pi 4)))
   (check-= (vector-ref t 0) .2 e)
   (check-= (vector-ref t 1) .2 e)
   (check-= err 0.0 e))

  (test-case
   "golden-section-search find minimum"
   (define-values (pt val)
     (golden-section-search
      (lambda (x) (let ([y (* (- x 2) (- x 2))]) (values y (abs (- y 0))))) -3 5 e))
   (check-= pt 2.0 (sqrt e))
   (check-= val 0.0 e))
  )
