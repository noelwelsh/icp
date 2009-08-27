#lang scheme/base

;; This probability model converts a match into a
;; probability as follows:
;;
;; We compute the normalised error between two scans. This
;; is the sum of squared error between each matching point
;; divided by the number of matches. Hence scans with few
;; matching points do not appear to have lower error than
;; scans with many matches
;;
;; We assume this error is distributed according to an
;; exponential distribution. We don't have any good cause to
;; believe this, but it makes the model tractable. We assume
;; the parameter of the exponential is distributed according
;; to a Gamma distribution. We can they analytically solve
;; the posterior, and, crucially, the indefinite integral
;; for the marginal distribution over error. When then
;; numerically find a value for lambda to solve the definite
;; integral.
;;
;; This models needs to be tested to see if the above
;; assumptions are realistic


(require
 scheme/match
 (planet williams/science:3/random-distributions/gaussian)
 "error-cache.ss")


;; struct cluster : (Listof Number) cache
(define-struct cluster (idxs cache))


;; cluster Number -> Number
;;
;; Likelihood of a number (error) under the
;; exponential/gamma model. I.e. marginalise out the lambda
;; parameter, giving p(x | alpha, beta)
;;
;; The integral, using Wolfram's integration tool, is:
;;
;; 1 \ gamma(lambda) [beta^alpha lambda^(alpha+1) gamma(alpha+1, lambda(beta + x)) ((beta + x) lambda)^(-alpha - 1)]
'(define (cluster-likelihood c error)
  (match-define (struct cluster [a b]) c)
  (define l (/ a (* b b))) ;; Three times the variance. A hack
  (/ (* (expt b a)
        (expt l (add1 a))
        (gamma-inc (add1 a) (* l (+ b error)))
        (expt (* l (+ b error)) (- (- a) l)))
     (gamma a)))

'(define (cluster-likelihood c error)
  (match-define (struct cluster [a b]) c)
  (define mean (/ a b))
  (define var (/ a (* b b)))
  (define step (/ (* 3 var) 10))
  (define min (if (<= (- mean (* 3 var)) 0)
                  (+ 0 step)
                  (- mean (* 3 var))))
  (define max (+ mean (* 3 var)))
  (define-values (p n)
    (for/fold ([p 0] [n 0])
        ([precision (in-range min max step)])
      ;;(printf "~a / ~a / ~a / ~a / ~a ~a ~a ~a\n" p n precision min max step a b)
      (values
       (+ (* (gaussian-pdf error 0 (/ 1 precision))
             (gamma-pdf precision a b))
          p)
       (add1 n))))
  (printf "error: ~a  a: ~a  b: ~a\n" error a b)
  (printf "mean sigma: ~a  p(error|mean) ~a\n" (exact->inexact (/ mean)) (gaussian-pdf error 0 (/ mean)))
  (printf "p(mean) ~a\n" (gamma-pdf mean a b))
  (/ p n)
  (gaussian-pdf error 0 (/ mean)))


(define (cluster-likelihood c idx)
  (match-define (struct cluster [idxs cache]) c)
  (define-values (sum n)
    (for/fold ([sum 0] [n 0])
        ([i (in-list idxs)])
      (values
       (+ sum (cache-ref c idx i))
       (add1 n))))
  (gaussian-pdf (/ sum n) 0 (cache-std-dev c)))


;; cluster Number -> cluster
;;
;; Posterior update for a cluster
(define (cluster-add c idx)
  (match-define (struct cluster [idxs cache]) c)
  (if (memq idx idxs)
      c
      (make-cluster (cons idx idxs) c)))

;; cluster Number -> cluster
(define (cluster-remove c idx)
  (match-define (struct cluster [idxs cache]) c)
  (make-cache (remq idx idxs) cache))


(provide
 (struct-out cluster)
 cluster-likelihood
 cluster-add
 cluster-remove)
