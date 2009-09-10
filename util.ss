#lang scheme/base

(require
 scheme/foreign
 "c/base.ss")


(define-icp (sse1
             "sse1"
             (err : (_vector i _double*)) (_int = (vector-length err)) -> _double))

(define-icp (sse2
             "sse2"
             (err1 : (_vector i _double*)) (_vector i _double*) (_int = (vector-length err1)) -> _double))
  

(define (square x) (* x x))

;; (Vectorof Number) -> Number
;; (Vectorof Number) (Vectorof Number) -> Number
;;
;; Calculate the sum of squared error.
;;
;; For single data set uses a more numerically stable
;; "shortcut" algorithm:
;;
;; sum([X - mean(X)]^2) = sum(X^2) - (sum(X)^2 / N)
;;
;; For the two argument case:
;; sum((x - mean(X))(y - mean(Y))) =
;;   sum(xy) - mean(X)sum(y) - mean(y)sum(x) + N mean(X)mean(Y)
(define sse
  (case-lambda
    [(data) (sse1 data)]
    [(data1 data2) (sse2 data1 data2)]))

;; Number Number Number Number -> (values Number Number Number Number)
;;
;; Calculates the inverse of a 2x2 matrix
(define (2x2-inverse a b c d)
  (define divisor (- (* a d) (* b c)))
  (values (/ d divisor)     (/ (- b) divisor)
          (/ (- c) divisor) (/ a divisor)))

(provide
 square
 sse
 2x2-inverse)