#lang scheme/base

;; The Iterative Dual Correspondence algorithm, combining
;; ICP and IMRP

(require "imrp.ss"
         "icp.ss")


(define (idc-iteration ref-pts ref-pose new-pts new-pose)
  (define-values (xt1 yt1 r1)
    (icp ref-pts new-pts ... ... ... ...))
  (define-values (xt2 yt2 r2)
    (imrp ref-pts new-pts ... ... ... ...))
  (values xt1 yt1 r2))
  