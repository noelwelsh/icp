#lang scheme/base

(require scheme/match
         "point.ss")

;; Point (Vectorof Point) Number -> Point
;;
;; pts must be sorted by angle from low to high
(define (matching-point p pts rotation)
  (match-define (vector r a) p)
  (define low (- a rotation))
  (define high (+ a rotation))

  (for/fold ([found #f])
      ([p1 (in-vector pts)]
       [p2 (in-vector pts 1)])
    (match-define (vector r1 a1) p1)
    (match-define (vector r2 a2) p2)

    (if (or
         (and (< a1 low) (< a2 low))
         (and (> a1 high) (> a2 high)))
        ;; Current points don't overlap allowable range
        found
        (let* ([low-p (if (< a1 low)
                          (interpolate-point-to-angle p1 p2 low)
                          p1)]
               [high-p (if (< high a2)
                           (interpolate-point-to-angle p1 p2 high)
                           p2)]
               [closest (closest-point p low-p high-p)])
          (cond
           ;; We haven't previously found a point, so go with the one we just found
           [(not found) closest]
           ;; Choose the point with a range closer to p
           [(< (abs (- (point-r closest) r))
               (abs (- (point-r found) r)))
            closest]
           ;; If equal ranges, choose point with angle closer to p
           [(= (point-r closest) (point-r found))
            (if (< (abs (- (point-a closest) a))
                   (abs (- (point-a found) a)))
                closest
                found)]
           [else found])))))
  

;; Point Point Point Number -> Point
(define (closest-point p p1 p2)
  (match-define (vector r a) p)
  (match-define (vector r1 a1) p1)
  (match-define (vector r2 a2) p2)

  (cond
   [(and (< r1 r) (< r2 r))
    (if (> r1 r2)
        p1
        p2)]
   [(and (> r1 r) (> r2 r))
    (if (< r1 r2)
        p1
        p2)]
   [else
    (interpolate-point-to-range p1 p2 r)]))

(define (interpolate-point-to-angle p1 p2 a)
  (match-define (vector r1 a1) p1)
  (match-define (vector r2 a2) p2)

  (define r
    (/ (* r1 r2 (- a2 a1))
       (+ (* r1 (- a a1)) (* r2 (- a2 a)))))
  (make-point r a))

(define (interpolate-point-to-range p1 p2 r)
  (match-define (vector r1 a1) p1)
  (match-define (vector r2 a2) p2)

  (define a
    (* (/ (- r1 r2))
       (+ (/ (* r1 r2 (- a2 a1)) r)
          (- (* r1 a1) (* r2 a2)))))
  (make-point r a))

(provide
 matching-point
 closest-point
 interpolate-point-to-range
 interpolate-point-to-angle)