#lang scheme/base

;;; This is the complete scan matching algorithm, which
;;; consists of the SLSMA and IDC algorithms.

(require
 (planet schematics/numeric:1/for)
  "slsma.ss"
  "idc.ss"
  "geometry.ss")

;; Default parameters

;; For SLSMA
(define occlusion-angle .001) ;; About .0625 degrees
(define neighbourhood 5)
(define angle-limit .7)
(define error-limit 100)
(define alpha .7)
(define Hd 100)
(define rotation-min -.4) ;; About 25 degrees
(define rotation-max .4)
(define tolerance 0.001)

;; For IDC
(define rotation .2) ;; About 12 degrees


;; (Vectorof Polar) Pose (Vectorof Polar) Pose -> (values Number Number Number)
(define (scan-match ref-pts ref-pose new-pts new-pose)
  (define proj-pts (project-points ref-pts ref-pose new-pose))
  (define-values (xt yt a) (slsma ref-pts ref-pose new-pts new-pose
                                  occlusion-angle
                                  neighbourhood angle-limit error-limit
                                  alpha Hd
                                  rotation-min rotation-max tolerance))
  ;;(printf "scan-match slsma: ~a ~a ~a\n" xt yt a)
  (idc proj-pts new-pts xt yt a rotation))

;; (Vectorof Polar) Pose (Vectorof Polar) Pose -> (values Number Number Number)
;;
;; Scan matching using just IDC
(define (scan-match/idc ref-pts ref-pose new-pts new-pose)
  (define proj-pts (project-points ref-pts ref-pose new-pose))
  (idc proj-pts new-pts 0 0 0 rotation))


(provide
 scan-match
 scan-match/idc
 rotation)
