#lang scheme/base

(require scheme/match
         scheme/math
         (planet schematics/numeric:1/vector)
         (planet schematics/numeric:1/for)
         (planet schematics/numeric:1/matrix)
         (planet schematics/mzgsl:1/gslvector)
         (planet schematics/mzgsl:1/linear-least-squares)
         (planet williams/science:3/statistics)
         "point.ss"
         "pose.ss"
         "tangent.ss"
         "util.ss")

;; The "search/least-squares matching algorithm"

;; Polar Pose Pose -> Polar
(define (project-point pt ref-pose new-pose)
  (match-define (struct pose [r-x r-y r-a]) ref-pose)
  (match-define (struct pose [n-x n-y n-a]) new-pose)
  ;; Point in world coordinates
  (define world-pt
    (cartesian+ (polar->cartesian (polar-rotate pt r-a))
                (make-cartesian r-x r-y)))
  ;; Create the new basis
  (define new-x (polar->cartesian (make-polar 1 n-a)))
  (define new-y (polar->cartesian (polar-rotate (make-polar 1 n-a) (/ pi 2))))
  (define t (matrix-invert
             (matrix 2 2
                     (cartesian-x new-x) (cartesian-x new-y)
                     (cartesian-y new-x) (cartesian-y new-y))))
  ;; Transform world point to new basis
  (define new-pt
    (let* ([pt (cartesian- world-pt (make-cartesian n-x n-y))]
           [v (matrix*v t (vector (cartesian-x pt) (cartesian-y pt)))])
      (make-cartesian (vector-ref v 0) (vector-ref v 1))))
  (define r (cartesian-distance new-pt (make-cartesian 0 0)))
  (define theta (atan (cartesian-y new-pt) (cartesian-x new-pt)))
  ;;(printf "world-pt: ~a\nnew-x: ~a\nnew-y: ~a\nnew-pt: ~a\nr: ~a\ntheta: ~a\n" world-pt new-x new-y new-pt r theta)
  (make-polar r
              (if (< theta 0)
                  (+ theta (* 2 pi))
                  theta)))
  
;; (Vectorof Polar) Pose Pose -> (Vectorof Polar)
;;
;; pts must be ordered by angles smallest to largest
(define (project-points pts ref-pose new-pose)
  ;; This is faster than calling project-point for every point
  (match-define (struct pose [r-x r-y r-a]) ref-pose)
  (match-define (struct pose [n-x n-y n-a]) new-pose)
  (define new-x (polar->cartesian (make-polar 1 n-a)))
  (define new-y (polar->cartesian (polar-rotate (make-polar 1 n-a) (/ pi 2))))
  (define t (matrix-invert
             (matrix 2 2
                     (cartesian-x new-x) (cartesian-x new-y)
                     (cartesian-y new-x) (cartesian-y new-y))))
  
  (for/vector ([i (vector-length pts)]
               [pt (in-vector pts)])
              (define world-pt (cartesian+ (polar->cartesian (polar-rotate pt r-a))
                                           (make-cartesian r-x r-y)))
              (define new-pt
                (let* ([pt (cartesian- world-pt (make-cartesian n-x n-y))]
                       [v (matrix*v t (vector (cartesian-x pt) (cartesian-y pt)))])
                  (make-cartesian (vector-ref v 0) (vector-ref v 1))))
              (define r (cartesian-distance new-pt (make-cartesian 0 0)))
              (define theta (atan (cartesian-y new-pt) (cartesian-x new-pt)))
              (make-polar r
                          (if (< theta 0)
                              (+ theta (* 2 pi))
                              theta))))

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


;; Polar Polar (Vectorof Polar) (Vectorof (U Polar #f)) Number Number Number
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
       [n2 (in-vector normals 1)]
       #:when (and n1 n2))
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
          ;;(printf "~a ~a ~a ~a ~a ~a ~a ~a\n" percentage r* n*-r n*-a p* n* divergence distance)
          (if (and (<= (abs distance) Hd)
                   (<= (cos alpha) divergence))
              (values p* n*)
              (values found-pt found-normal))))))

;; (Vectorof Polar) (Vectorof (U Polar #f))
;; (Vectorof Polar) (Vectorof (U Polar #f))
;; Number Number Number
;;   ->
;; (values (Vectorof (U Polar #f)) (Vectorof (U Polar #f)))
(define (matching-points pts normals ref-pts ref-normals rotation alpha Hd)
  (define n-points (vector-length pts))
  (for/vector ([i n-points 2]
               [pt (in-vector pts)]
               [n  (in-vector normals)])
    (if n
        (matching-point pt n ref-pts ref-normals rotation alpha Hd)
        (values #f #f))))


;; (Vectorof Polar) (Vectorof (U Polar #f)) (Vectorof (U Polar #f)) (Vectorof (U Polar #f)) Number ->
;;   (values (Vector Number Number) Number)
;;
;; Compute the least squares optimal translation given
;; points and their matches
;;
;; Returns least squares solution and error
(define (optimise-translation points normals matches matching-normals rotation)
  (define n-pts (for/sum ([p (in-vector matches)]) (if p 1 0)))
  (define x (make-matrix n-pts 2))
  (define y (make-gslvector n-pts))  

  (for/fold ([i 0])
      ([p  (in-vector points)]
       [n  (in-vector normals)]
       [p* (in-vector matches)]
       [n* (in-vector matching-normals)])
    (if (and n p* n*)
        (let* ([d (polar-dot (polar+ (polar-rotate n rotation) n*)
                             (polar- p* (polar-rotate p rotation)))]
               [c (polar->cartesian (polar+ (polar-rotate n rotation) n*))]
               [cx (cartesian-x c)]
               [cy (cartesian-y c)])
          (matrix-set! x i 0 cx)
          (matrix-set! x i 1 cy)
          (gslvector-set! y i d)
          (add1 i))
        i))
  (let-values (([t cov err] (least-squares-multiparameter x y)))
    (values (gslvector->vector t) err)))

;; ('a -> (values 'a Number)) Number Number Number -> (values Number 'a)
(define (golden-section-search solve start-r end-r tolerance)
  ;; Golden ration conjugate
  (define Phi (- (/ (+ 1 (sqrt 5)) 2) 1))
  (define mid (+ start-r (* (- 1 Phi) (- end-r start-r))))
  (define accuracy (* tolerance (- end-r start-r)))
  (define-values (start-soln start-err) (solve start-r))
  (define-values (end-soln end-err) (solve end-r))
  (define-values (mid-soln mid-err) (solve mid))
  
  (let loop ([start start-r] [start-soln start-soln] [start-err start-err]
             [mid mid] [mid-soln mid-soln] [mid-err mid-err]
             [end end-r] [end-soln end-soln] [end-err end-err])
    (define new (- end (- mid start)))
    (define-values (new-soln new-err) (solve new))
    ;;(printf "golden-section-search: ~a ~a ~a: ~a\n" start mid end (- end start))
    (cond
     [(< (- end start) accuracy)
      (values mid mid-soln)]
     [(< new-err mid-err)
      (if (< mid new)
          (loop mid mid-soln mid-err new new-soln new-err end end-soln end-err)
          (loop start start-soln start-err new new-soln new-err mid mid-soln mid-err))]
     [(>= new-err mid-err)
      (if (< mid new)
          (loop start start-soln start-err mid mid-soln mid-err new new-soln new-err)
          (loop new new-soln new-err mid mid-soln mid-err end end-soln end-err))])))

;; ... -> (values Number (Vector Number Number))
;;
;; Returns optimal rotation and transformation
(define (optimal-transformation new-pts new-pose ref-pts ref-pose
                                occlusion-angle
                                neighbourhood angle-limit error-limit
                                alpha Hd
                                rotation-min rotation-max tolerance)
  (let* ([proj-pts (project-points ref-pts ref-pose new-pose)]
         [filtered-proj-pts (filter-points proj-pts new-pts occlusion-angle)]
         ;; (U Polar #f)
         [ref-norms (fit-tangents (vector-map polar->cartesian filtered-proj-pts)
                                  neighbourhood angle-limit error-limit)]
         ;; (U Polar #f)
         [new-norms (fit-tangents (vector-map polar->cartesian new-pts)
                                  neighbourhood angle-limit error-limit)])
    (define (solve rotation)
      ;; (U Polar #f) (U Polar #f)
      (define-values (matches normals)
        (matching-points new-pts new-norms filtered-proj-pts ref-norms
                         rotation alpha Hd))
      (optimise-translation new-pts new-norms matches normals rotation))
    (golden-section-search solve rotation-min rotation-max tolerance)))


(provide
 project-point
 project-points
 filter-points

 filter-bounded-obstacle
 filter-opaque

 matching-point
 matching-points

 optimise-translation

 golden-section-search
 
 optimal-transformation)