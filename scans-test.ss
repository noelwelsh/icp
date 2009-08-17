#lang scheme/base

;; Functional test using the scans in scans.ss

(require (planet schematics/numeric:1/bonfire/frame)
         (planet schematics/numeric:1/bonfire/screen)
         "pose.ss"
         "point.ss"
         "scans.ss"
         "slsma.ss")

;; Pose (Vectorof Polar) -> Frame
(define (pose+points->frame pose points)
  (dots
   (for/list ([pt (in-vector points)])
     (let ([p (cartesian+ (polar->cartesian pt)
                          (make-cartesian (pose-x pose) (pose-y pose)))])
       (vector-immutable (cartesian-x p) (cartesian-y p))))))

;; (Listof Number) -> Frame
(define (scan->frame scan)
  (define-values (pose points) (process-scan scan))
  (pose+points->frame pose points))

(define frame1
  (colour (scan->frame scan1) (vector-immutable 255 0 0)))

(define frame2
  (colour (scan->frame scan2) (vector-immutable 0 255 0)))

(define frame3
  (let ()
    (define-values (ref-pose ref-pts) (process-scan scan1))
    (define-values (new-pose new-pts) (process-scan scan2))
    (define-values (opt-r opt-t)
      (optimal-transformation new-pts new-pose ref-pts ref-pose
                              .004 ;; About .25 degrees
                              5 1.05 5
                              1 5
                              -.5 .5 238))
    (define opt-pose
      (make-pose (+ (pose-x new-pose) (vector-ref opt-t 0))
                 (+ (pose-y new-pose) (vector-ref opt-t 1))
                 (+ (pose-a new-pose) opt-r)))
    (printf "Found optimal translation of ~a and rotation of ~a\n" opt-t opt-r)
    (colour (pose+points->frame opt-pose new-pts) (vector-immutable 0 0 255))))


(plot-screen "Aligned Scans" (overlay frame1 (overlay frame2 frame3)))