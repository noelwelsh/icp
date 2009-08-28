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


;; cache -> cluster
(define (create-cluster cache)
  (make-cluster null cache))

;; cluster Number -> Number
;;
;; Likelihood of a number (error) under the
;; exponential/gamma model. I.e. marginalise out the lambda
;; parameter, giving p(x | alpha, beta)
;;
;; The integral, using Wolfram's integration tool, is:
;;
;; 1 \ gamma(lambda) [beta^alpha lambda^(alpha+1) gamma(alpha+1, lambda(beta + x)) ((beta + x) lambda)^(-alpha - 1)]



(define (cluster-likelihood c idx)
  (match-define (struct cluster [idxs cache]) c)
  (define-values (sum n)
    (for/fold ([sum 0] [n 0])
        ([i (in-list idxs)])
      (values
       (+ sum (cache-ref cache idx i))
       (add1 n))))

  (if (zero? n)
      (gaussian-pdf (cache-std-dev cache) 0 (cache-std-dev cache)) 
      (gaussian-pdf (/ sum n) 0 (cache-std-dev cache))))


;; cluster Number -> cluster
;;
;; Posterior update for a cluster
(define (cluster-add c idx)
  (match-define (struct cluster [idxs cache]) c)
  (if (memq idx idxs)
      c
      (make-cluster (cons idx idxs) cache)))

;; cluster Number -> cluster
(define (cluster-remove c idx)
  (match-define (struct cluster [idxs cache]) c)
  (make-cluster (remq idx idxs) cache))


(provide
 (struct-out cluster)
 create-cluster
 cluster-likelihood
 cluster-add
 cluster-remove)
