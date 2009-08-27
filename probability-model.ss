#lang scheme/base

(require
  (prefix-in icp: "icp.ss")
  (prefix-in imrp: "imrp.ss"))

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



;; struct cluster : Number Number Number
;;
;; a is the alpha parameter of the gamma prior
;; b is the beta parameter of the gamma prior
(define-struct cluster (a b))



;; (Vectorof Polar) (Vectorof Polar) Number Number Number -> Number
;;
;; Returns the sum of squared error of translated point
;; matches to reference points using both ICP and IMRP
;; normalised by the number of matching points
;;
;; ref-pts should be projected to the same frame of
;; reference as new-pts
(define (normalised-error ref-pts new-pts xt yt a)
  (define transformed-pts
    (vector-map
     (lambda (pt)
       (cartesian->polar (cartesian-transform (polar->cartesian pt) xt yt a)))
     new-pts))
  (define icp-matches
    (idc:matching-points ref-pts transformed-pts rotation))
  (define imrp-matches
    (imrp:matching-points ref-pts transformed-pts rotation))
  (define (error pt1 pt2)
    (if (and pt1 pt2)
        (values (cartesian-distance (polar->cartesian pt1) (polar->cartesian pt2)) 1)
        (values 0 0)))
  (define-values (err n)
    (for/fold ([err 0] [n 0])
        ([pt1 (in-vector transformed-pts)]
         [pt2 (in-vector icp-matches)]
         [pt3 (in-vector imrp-matches)])
      (define-values (err1 n1)
        (error pt1 pt2))
      (define-values (err2 n2)
        (error pt1 pt3))
      (values (+ (* err1 err1) (* err2 err2) err) (+ n1 n2 n))))
  (/ err n))


;; cluster Number -> Number
;;
;; Likelihood of a number (error) under the
;; exponential/gamma model. I.e. marginalise out the lambda
;; parameter, giving p(x | alpha, beta)
;;
;; The integral, using Wolfram's integration tool, is:
;;
;; 1 \ gamma(lambda) [beta^alpha lambda^(alpha+1) gamma(alpha+1, lambda(beta + x)) ((beta + x) lambda)^(-alpha - 1)]
(define (cluster-likelihood c error)
  (match-define (struct cluster [a b]) c)
  (define l (/ a (* b b))) ;; Three times the variance. A hack
  (/ (* (expt b a)
        (expt l (add1 a))
        (gamma-inc (add1 a) (* l (+ beta error)))
        (expt (* l (+ b err)) (- (- a) l)))
     (gamma a)))


;; cluster Number -> cluster
;;
;; Posterior update for a cluster
(define (cluster-add c error)
  (match-define (struct cluster [a b]) c)
  (make-cluster (add1 a) (+ b error)))

;; cluster Number -> cluster
(define (cluster-remove c error)
  (match-define (struct cluster [a b]) c)
  (make-cluster (sub1 a) (- b error)))

(provide
 (struct-out cluster)
 cluster-likelihood
 cluster-add
 cluster-remove
 normalised-error)