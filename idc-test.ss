#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/vector)
         "geometry.ss"
         "point.ss"
         "idc.ss"
         "austin-laser-points.ss")

(define/provide-test-suite idc-tests
  (test-case
   "Iterations of IDC converge to true transform"
   (define ref-pts (vector-map
                    (lambda (pt) (polar-normalise (cartesian->polar pt)))
                    (make-ellipse-points 3 3 1000 500 .1 .1)))
   (define new-pts (vector-map
                    (lambda (pt) (polar-normalise (cartesian->polar pt)))
                    (make-ellipse-points 6 6 1000 500 .3 .1)))
   (define-values (true-xt true-yt true-a) (values -3 -3 -.2))
   (define-values (xt yt a) (idc ref-pts new-pts -3 -3 -.2 .1))
   (printf "IDC: ~a ~a ~a\n" xt yt a)
   (check < (abs (- true-xt xt)) 0.001)
   (check < (abs (- true-yt yt)) 0.001)
   (check < (abs (- true-a a)) 0.001))

  (test-case
   "idc aligns identical points without any transformation"
   (for ([pts (in-vector points)]
         [pose (in-vector poses)]
         [i (in-naturals)])
        (define ref-pts (project-points pts pose pose))
        (define new-pts pts)
        (define-values (xt yt a) (idc ref-pts new-pts 0 0 0 .2))
        (printf "IDC ~a: ~a ~a ~a\n" i xt yt a)
        (check-= xt 0 0.1)
        (check-= yt 0 0.1)
        (check-= a  0 0.1)))
  )