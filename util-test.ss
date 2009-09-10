#lang scheme/base

(require (planet schematics/schemeunit:3/test)
         "util.ss")

(define/provide-test-suite util-tests
  (test-case
   "sse"
   (check-= (sse (vector 4 5 3 6 5)) 5.2 0.00001))

  (test-case
   "sse, two data sets"
   (check-= (sse (vector 4 5 3 6 5)
                 (vector 3 5 7 3 2))

            (apply + (map (lambda (x y)
                            (* (- x 23/5) (- y 4)))
                          (list 4 5 3 6 5)
                          (list 3 5 7 3 2)))
            0.00001))

  (test-case
   "2x2-inverse"
   (define-values (a b c d)
     (2x2-inverse 3 1 4 2))
   (check-= a 1 0)
   (check-= b -1/2 0)
   (check-= c -2 0)
   (check-= d 3/2 0))
  )