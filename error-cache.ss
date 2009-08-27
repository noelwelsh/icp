#lang scheme/base

;; Int the iTM application (and probably others) there will
;; be repeated comparisons between scans for the purpose of
;; inferring a probability model. This is potentially a lot
;; of wasted computation, as the scans aren't changing. This
;; module caches the error calculations to they aren't
;; repeated.

(require
 (planet schematics/numeric:1/vector)
 (planet schematics/numeric:1/matrix)
 "scan-match.ss"
 "point.ss"
 (prefix-in icp: "icp.ss")
 (prefix-in imrp: "imrp.ss"))


;; struct cache: Matrix Number
(define-struct cache (errors std-dev))

;; (Vectorof (Vectorof Polar)) (Vectorof Pose) -> cache
(define (create-cache scans poses)
  (define l (vector-length scans))
  (define c (make-matrix l l))

  ;; We compute the full matrix, rather than a triangular
  ;; section, as it is unlikely that the scan matching is
  ;; symmetric.
  (define total-error
    (for*/fold ([sum 0])
               ([i (in-range l)]
                [j (in-range l)])
      (define ref-pts (vector-ref scans i))
      (define new-pts (vector-ref scans j))
      (define ref-pose (vector-ref poses i))
      (define new-pose (vector-ref poses j))
      
      (define-values (xt yt a)
        (scan-match ref-pts ref-pose new-pts new-pose rotation))
      (define error (normalised-error ref-pts new-pts xt yt a))
      (matrix-set! c i j error)
      (+ error sum)))

  (make-cache c (/ total-error (* l l))))

(define (cache-ref c ref-idx new-idx)
  (matrix-ref (cache-errors c) ref-idx new-idx))



;; Utilities


;; (Vectorof Polar) (Vectorof Polar) Number Number Number Number -> Number
;;
;; Returns the sum of squared error of translated point
;; matches to reference points using both ICP and IMRP
;; normalised by the number of matching points
;;
;; ref-pts should be projected to the same frame of
;; reference as new-pts
(define (normalised-error ref-pts new-pts xt yt a rotation)
  (define transformed-pts
    (vector-map
     (lambda (pt)
       (cartesian->polar (cartesian-transform (polar->cartesian pt) xt yt a)))
     new-pts))
  (define icp-matches
    (icp:matching-points ref-pts transformed-pts rotation))
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


(provide
 (struct-out cache)
 create-cache
 cache-ref)