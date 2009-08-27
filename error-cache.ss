#lang scheme/base

;; Int the iTM application (and probably others) there will
;; be repeated comparisons between scans for the purpose of
;; inferring a probability model. This is potentially a lot
;; of wasted computation, as the scans aren't changing. This
;; module caches the error calculations to they aren't
;; repeated.

(require
 (planet schematics/numeric:1/matrix)
 "scan-match.ss"
 "probability-model.ss")


;; struct cache: Matrix
(define-struct cache (errors))

;; (Vectorof (Vectorof Polar)) (Vectorof Pose) -> cache
(define (create-cache scans poses)
  (define l (vector-length scans))
  (define c (make-matrix l l))

  ;; We compute the full matrix, rather than a triangular
  ;; section, as it is unlikely that the scan matching is
  ;; symmetric.
  (for* ([i (in-range l)]
         [j (in-range l)])
    (define ref-pts (vector-ref scans i))
    (define new-pts (vector-ref scans j))
    (define ref-pose (vector-ref poses i))
    (define new-pose (vector-ref poses j))
    
    (define-values (xt yt a) (scan-match ref-pts ref-pose new-pts new-pose))
    (matrix-set! c i j (normalised-error ref-pts new-pts xt yt a)))

  (make-cache c))

(define (cache-ref c ref-idx new-idx)
  (matrix-ref (cache-error c) ref-idx new-idx))


(provide
 (struct-out cache)
 create-cache
 cache-ref)