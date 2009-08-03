#lang scheme/base

(require scheme/match
         (planet schematics/numeric:1/vector)
         (planet williams/science:3/statistics)
         "point.ss"
         "util.ss")

;; The "search/least-squares matching algorithm"

;; (Vectorof Polar) Cartesian Cartesian -> (Vectorof Polar)
;;
;; pts must be ordered by angles smallest to largest
(define (project-points pts ref-pose new-pose)
  (for/vector ([i (vector-length pts)]
               [pt (in-vector pts)])
              (cartesian->polar
               (cartesian- (cartesian+ (polar->cartesian pt) ref-pose)
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
     (if (> (polar-a p1) (polar-a p2))
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
            (if (and (< (abs (- (polar-a pt) (polar-a p1))) angle)
                     (< (polar-r p1) (polar-r pt)))
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


;; Polar Polar (Vectorof Polar) (Vectorof Polar) Number Number Number
;;   -> (values (U Polar #f) (U Polar #f)
;;
;; Finds the closest matching point and normal, or #f if no
;; points matches
(define (matching-point pt normal pts normals rotation alpha Hd)
  (match-define (struct polar (r a)) pt)
  (define (interpolate a b percentage)
    (+ (* (- a b) percentage) b))
  (define angle (+ a rotation))

  (for/fold ([found-pt #f] [found-normal #f])
      ([p1 (in-vector pts)]
       [n1 (in-vector normals)]
       [p2 (in-vector pts 1)]
       [n2 (in-vector normals 1)])
    (match-define (struct polar (r1 a1)) p1)
    (match-define (struct polar (r2 a2)) p2)

    (if (or
         (and (< a1 angle) (< a2 angle))
         (and (> a1 angle) (> a2 angle)))
        ;; Current points don't straddle the angle
        (values found-pt found-normal)
        ;; Interpolate a point and normal to match pt's angle
        (let* ([percentage (/ (- angle a1) (- a2 a1))]
               [r* (interpolate r2 r1 percentage)]
               [n*-r (interpolate (polar-r n2) (polar-r n1) percentage)]
               [n*-a (interpolate (polar-a n2) (polar-a n1) percentage)]
               [p* (make-polar r* angle)]
               [n* (make-polar r a)]
               ;; The angle between the rotated normal and the interpolated normal
               [divergence (polar-dot (polar-rotate normal rotation) n*)]
               [distance (polar-dot (polar+ (polar-rotate normal rotation) n*)
                                    (polar- p* (polar-rotate pt rotation)))])
          (if (or (<= (abs distance) Hd)
                  (<= (cos alpha) divergence))
              (values found-pt found-normal)
              (values p* n*))))))

  
(provide
 project-points
 filter-points

 filter-bounded-obstacle
 filter-opaque)