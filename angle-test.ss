#lang scheme/base

(require scheme/math
         (planet schematics/schemeunit:3/test)
         "angle.ss")

(define 2pi (* 2 pi))


(define/provide-test-suite angle-tests
  (test-case
   "angle-normalise"
   (check-= (angle-normalise 0) 0 0)
   (check-= (angle-normalise 1) 1 0)
   (check-= (angle-normalise 2) 2 0)
   (check-= (angle-normalise 2pi) 0 0)
   (check-= (angle-normalise (+ 2pi 1)) 1 0)
   (check-= (angle-normalise (* 2pi 2)) 0 0)

   (check-= (angle-normalise -1) (- 2pi 1) 0)
   (check-= (angle-normalise -2) (- 2pi 2) 0)
   (check-= (angle-normalise -8) (+ -8 2pi 2pi) 0)
   (check-= (angle-normalise (- 2pi)) 0 0)
   (check-= (angle-normalise (* -4 2pi)) 0 0)
   (check-= (angle-normalise (- (* -2 2pi) 2)) (- 2pi 2) 0))

  (test-case
   "angle=?"
   (check angle=? 0 0)
   (check angle=? 1 1)
   (check-true (angle=? 0 .1 .1))
   (check-false (angle=? 0 .2 .1))
   (check-true (angle=? 0 (* 1.9 pi) (* .1 pi)))
   (check-false (angle=? 0.1 (* 1.9 pi) .1)))

  (test-case
   "angle<?"
   (check angle<? 0 1)
   (check angle<? 0 (- pi .1))
   (check-false (angle<? 1 0))
   (check-false (angle<? 1 (- 2pi 1)))
   (check-false (angle<? 0 (+ pi .1))))

  )