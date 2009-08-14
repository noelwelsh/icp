#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "slsma.ss"
         "point.ss"
         "pose.ss")

(define e 0.00001)

(define/provide-test-suite slsma-tests
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
   (define t (optimise-translation pts pts t-pts t-pts (/ pi 4)))
   (check-= (vector-ref t 0) .2 e)
   (check-= (vector-ref t 1) .2 e))
  )
