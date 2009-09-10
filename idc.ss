#lang scheme/base

;; The Iterative Dual Correspondence algorithm, combining
;; ICP and IMRP

(require (only-in "icp.ss" icp)
         (only-in "imrp.ss" imrp))

;; (Vectorof Polar) (Vectorof Polar) Number Number Number Number
;; [#:iterations Number] [#:threshold Number]
;;   ->
;; (values Number Number Number)
;;
;; The Iterated Dual Correspondence algorithm.
;;
;; xt, yt, and a are the starting transformation
;; (translation and rotation) Result is new 
;; translation and rotation, which includes xt, yt, and a.
;;
;; #:iterations is the maximum number of iterations to run for
;; #:threshold is the minimum change in all parameter of the transformation
;;
;; The algorithm halts when all parameters of the
;; transformation change by less than threshold, or
;; iterations is exceeded.
;;
;; ref-pts should be projected to the same frame of
;; reference as new-pts
(define (idc ref-pts new-pts xt yt a rotation
             #:iterations [iterations 100]
             #:threshold [threshold .001])
  (when (not (= (vector-length ref-pts) (vector-length new-pts)))
    (raise-mismatch-error
     'idc
     "Reference and new points do not have same length: "
     (cons (vector-length ref-pts) (vector-length new-pts))))
  
  (let loop ([i iterations] [xt xt] [yt yt] [a a] [rotation rotation])
    ;;(printf "IDC iteration ~a: ~a ~a ~a ~a\n" i xt yt a rotation)
    (if (zero? i)
        (values xt yt a)
        (let-values (([d-xt d-yt d-a]
                      (idc-iteration ref-pts new-pts xt yt a rotation)))
          ;;(printf "IDC d-xt: ~a  d-yt: ~a d-a: ~a\n" d-xt d-yt d-a)
          (if (and (< (abs d-xt) threshold) (< (abs d-yt) threshold) (< (abs d-a) threshold))
              (values xt yt a)
              (loop (sub1 i) (+ xt d-xt) (+ yt d-yt) (+ a d-a) rotation))))))
  

(define (idc-iteration ref-pts new-pts xt yt a rotation)
  (define-values (xt1 yt1 r1)
    (icp ref-pts new-pts xt yt a rotation))
  (define-values (xt2 yt2 r2)
    (imrp ref-pts new-pts xt yt a rotation))
  (values xt1 yt1 r2))



(provide idc)