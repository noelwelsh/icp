#lang scheme/base

;; The Iterative Dual Correspondence algorithm, combining
;; ICP and IMRP

(require "imrp.ss"
         "icp.ss")

(define (idc ref-pts new-pts xt yt a rotation)
  (define-values (next-tx next-ty next-r)
    (let-values (([ntx nty nr] (idc-iteration ref-pts new-pts tx ty a rotation)))
      (values (+ tx ntx) (+ ty nty) (+ r nr))))
  

(define (idc-iteration ref-pts new-pts xt yt a rotation)
  (define-values (xt1 yt1 r1)
    (icp ref-pts new-pts xt yt a rotation))
  (define-values (xt2 yt2 r2)
    (imrp ref-pts new-pts xt yt a rotation))
  (values xt1 yt1 r2))
  