#lang scheme/base

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
    [(data)
     (define n (vector-length data))
     (define-values (squares sum)
       (for/fold ([squares 0] [sum 0])
           ([x (in-vector data)])
         (values (+ squares (* x x))
                 (+ sum x))))
     (- squares (/ (* sum sum) n))]
    [(data1 data2)
     (define n (vector-length data1))
     (define-values (sum-xy sum-x sum-y)
       (for/fold ([sum-xy 0] [sum-x 0] [sum-y 0])
           ([x (in-vector data1)]
            [y (in-vector data2)])
         (values (+ sum-xy (* x y))
                 (+ sum-x x)
                 (+ sum-y y))))
     (define mean-x (/ sum-x n))
     (define mean-y (/ sum-y n))

     (+ sum-xy (- (* mean-x sum-y)) (- (* mean-y sum-x)) (* n mean-x mean-y))]))

(provide sse)