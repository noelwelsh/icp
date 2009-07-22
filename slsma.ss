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

;; (Vectorof Polar) -> (Vectorof Polar)
(define (filter-bounded-obstacle pts)
  (define n-ref-pts (vector-length pts))
  ;; Remove any points where angles go from large to small
  (vector-select
   pts
   (for/fold ([mask (make-vector n-ref-pts #t)])
       ([p1 (in-vector pts)]
        [p2 (in-vector pts 1)]
        [i  (in-naturals)])
     (if (> (point-a p1) (point-a p2))
         (begin (vector-set! mask i #f)
                (vector-set! mask (add1 i) #f)
                mask)
         mask))))


;; (Vectorof Polar) (Vectorof Polar) Number -> (Vectorof Polar)
;;
;; If two points p1 and p2 are within angle of one another,
;; the one with smaller r obscures the other
(define (filter-opaque ref-pts new-pts angle)
  (define n-ref-pts (vector-length ref-pts))
  (define (obscured? pt pts)
    (for/or ([p1 (in-vector pts)])
            (if (and (< (abs (- (point-a pt) (point-a p1))) angle)
                     (< (point-r p1) (point-r pt)))
                #t
                #f)))
  (vector-select
   ref-pts
   (for/fold ([mask (make-vector n-ref-pts #t)])
       ([pt (in-vector ref-pts)]
        [i (in-naturals)])
     (when (or (obscured? pt ref-pts)
               (obscured? pt new-pts))
       (vector-set! mask i #f))
     mask)))
  
;; (Vectorof Polar) (Vectorof Polar) Number -> (Vectorof Polar)
;;
;; If two points p1 and p2 are within angle of one another,
;; the one with smaller r obscures the other
(define (filter-points ref-pts new-pts angle)
  (filter-opaque (filter-bounded-obstacle ref-pts) new-pts angle))

(provide
 project-points
 filter-points

 filter-bounded-obstacle
 filter-opaque)