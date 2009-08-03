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
  )