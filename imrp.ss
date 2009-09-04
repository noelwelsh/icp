#lang scheme/base

(require scheme/match
         (planet schematics/numeric:1/vector)
         (planet schematics/numeric:1/for)
         (planet williams/science:3/statistics)
         "point.ss"
         "util.ss")

;; (Vectorof Polar) (Vectorof Polar) Number -> (Vectorof Polar)
(define (matching-points scan-pts model-pts rotation)
  (for/vector ([i (vector-length scan-pts)]
               [pt (in-vector scan-pts)])
              (matching-point pt model-pts rotation)))

;; Polar (Vectorof Polar) Number -> Polar
;;
;; pts must be sorted by angle from low to high
(define (matching-point p pts rotation)
  (match-define (struct polar (r a)) p)
  (define low (- a rotation))
  (define high (+ a rotation))

  (for/fold ([found #f])
      ([p1 (in-vector pts)]
       [p2 (in-vector pts 1)])
    (match-define (struct polar (r1 a1)) p1)
    (match-define (struct polar (r2 a2)) p2)

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
           [(< (abs (- (polar-r closest) r))
               (abs (- (polar-r found) r)))
            closest]
           ;; If equal ranges, choose point with angle closer to p
           [(= (polar-r closest) (polar-r found))
            (if (< (abs (- (polar-a closest) a))
                   (abs (- (polar-a found) a)))
                closest
                found)]
           [else found])))))
  

;; Polar Polar Polar Number -> Polar
(define (closest-point p p1 p2)
  (match-define (struct polar (r a)) p)
  (match-define (struct polar (r1 a1)) p1)
  (match-define (struct polar (r2 a2)) p2)

  (cond
   [(and (<= r1 r) (<= r2 r))
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
  (match-define (struct polar (r1 a1)) p1)
  (match-define (struct polar (r2 a2)) p2)

  (define r
    (/ (* r1 r2 (- a2 a1))
       (+ (* r1 (- a a1)) (* r2 (- a2 a)))))
  (make-polar r a))

(define (interpolate-point-to-range p1 p2 r)
  (match-define (struct polar (r1 a1)) p1)
  (match-define (struct polar (r2 a2)) p2)

  (define a
    (* (/ (- r1 r2))
       (+ (/ (* r1 r2 (- a2 a1)) r)
          (- (* r1 a1) (* r2 a2)))))
  (make-polar r a))


;; (Vectorof Polar) (Vectorof Polar) -> (values Number Number Number)
(define (optimal-transformation ref-pts match-pts)
  (define n-actual-matches (for/sum ([x (in-vector match-pts)]) (if x 1 0)))

  (if (or (zero? n-actual-matches) (= 1 n-actual-matches))
      (begin
        (display "IMRP: Zero or one matching points. Returning no transformation.\n")
        (values 0 0 0))
      (let ()
        (define-values (scan-pts matching-pts)
          (for/vector ([i n-actual-matches 2]
                       [p (in-vector ref-pts)]
                       [m (in-vector match-pts)]
                       #:when m)
                      (values p m)))

        (define s-pts (vector-map polar->cartesian scan-pts))
        (define m-pts (vector-map polar->cartesian matching-pts))

        (define s-xs (vector-map cartesian-x s-pts))
        (define m-xs (vector-map cartesian-x m-pts))
        (define s-ys (vector-map cartesian-y s-pts))
        (define m-ys (vector-map cartesian-y m-pts))
  
        (define s-x-mean (mean s-xs))
        (define m-x-mean (mean m-xs))
        (define s-y-mean (mean s-ys))
        (define m-y-mean (mean m-ys))

        (define Sxx (sse s-xs m-xs))
        (define Syy (sse s-ys m-ys))
        (define Sxy (sse s-xs m-ys))
        (define Syx (sse s-ys m-xs))

        (define angle (atan (/ (- Sxy Syx) (+ Sxx Syy))))
        (define t-x (- m-x-mean (* s-x-mean (cos angle)) (* s-y-mean (sin angle))))
        (define t-y (- m-y-mean (* s-x-mean (sin angle)) (* s-y-mean (cos angle))))

        (values t-x t-y angle))))

;; (Vectorof Polar) (Vectorof Polar) Number Number Number Number
;;   ->
;; (values Number Number Number)
;;
;; One iteration of the IMRP algorithm. We assume ref-pts
;; have already been projected to the same reference frame.
;;
;; xt, yt, and a are the starting transformation
;; (translation and rotation) Result is new found
;; translation and rotation, which should be applied in
;; addition to xt, yt, and a.
(define (imrp ref-pts new-pts xt yt a rotation)
  (define transformed-pts
    (vector-map
     (lambda (pt)
       (cartesian->polar (cartesian-transform (polar->cartesian pt) xt yt a)))
     new-pts))
  (define matching-pts
    (matching-points transformed-pts ref-pts rotation))
  (optimal-transformation transformed-pts matching-pts))


(provide
 matching-points
 matching-point
 closest-point
 interpolate-point-to-range
 interpolate-point-to-angle

 optimal-transformation

 imrp)