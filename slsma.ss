#lang scheme/base

(require (planet schematics/numeric:1/vector)
         "point.ss")

;; The "search/least-squares matching algorithm"

;; (Vectorof Polar) Cartesian Cartesian -> (Vectorof Polar)
;;
;; pts must be ordered by angles smallest to largest
(define (project-points pts ref-pose new-pose)
  (for/vector ([i (vector-length pts)]
               [pt (in-vector pts)])
              (cartesian->polar
               (point- (point+ (polar->cartesian pt) ref-pose)
                       new-pose))))

(define (filter-points pts)
  ;; Remove any points where angles go from large to small
  (define invisible
    (for/fold ([invisible null])
        ([p1 (in-vector pts)]
         [p2 (in-vector pts 1)])
      (if (> (point-a p1) (point-a p2))
          (list* p1 p2 invisible)
          invisible)))
                
  )