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
 "geometry.ss"
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
    (begin
      (printf "Building error cache\n")
      (for*/fold ([sum 0])
               ([i (in-range l)]
                [j (in-range l)])
      (define ref-pts (vector-ref scans i))
      (define new-pts (vector-ref scans j))
      (define ref-pose (vector-ref poses i))
      (define new-pose (vector-ref poses j))

      (printf "Iteration ~a ~a\n" i j)
      (if (= i j)
          (begin
            (matrix-set! c i j 0.0)
            sum)
          (let ([error (scan-match-error ref-pts ref-pose new-pts new-pose)])
            (printf "~a ~a ~a\n" i j error)
            (matrix-set! c i j error)
            (+ error sum))))))

  (when (zero? l)
    (raise-mismatch-error
     'create-cache "Cannot create error cache with zero scans:" scans))
  (make-cache c (/ total-error (* l l))))

(define (cache-ref c ref-idx new-idx)
  (matrix-ref (cache-errors c) ref-idx new-idx))


(define (write-cache cache file-name)
  (with-output-to-file file-name
    (lambda ()
      (define e (cache-errors cache))
      (define r (matrix-rows e))
      (define c (matrix-rows e))
      (printf "~a ~a\n" r c)
      (printf "~a\n" (cache-std-dev cache))
      (for ([i (in-range r)])
        (for ([j (in-range c)])
             (display (matrix-ref e i j)) (display " "))
        (newline)))
    #:exists 'replace))

(define (read-cache file-name)
  (with-input-from-file file-name
    (lambda ()
      (define r (read))
      (define c (read))
      (define std-dev (read))
      (define e (make-matrix r c))
      (for* ([i (in-range r)]
             [j (in-range c)])
            (matrix-set! e i j (read)))
      (make-cache e std-dev))))


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
        (cartesian-distance (polar->cartesian pt1) (polar->cartesian pt2))
        100))
  (define err
    (for/fold ([err 0])
        ([pt1 (in-vector transformed-pts)]
         [pt2 (in-vector icp-matches)]
         [pt3 (in-vector imrp-matches)])
      (define err1 (error pt1 pt2))
      (define err2 (error pt1 pt3))
      (+ (* err1 err1) (* err2 err2) err)))
  (/ err (* 2 (vector-length transformed-pts))))


(define (scan-match-error ref-pts ref-pose new-pts new-pose)
  (define-values (xt yt a)
    (scan-match ref-pts ref-pose new-pts new-pose))
  (define error
    (let ([proj-pts (project-points ref-pts ref-pose new-pose)])
      (normalised-error proj-pts new-pts xt yt a rotation)))
  error)

(provide
 (struct-out cache)
 create-cache
 cache-ref

 write-cache
 read-cache

 normalised-error
 scan-match-error)