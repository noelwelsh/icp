#lang scheme/base

(require scheme/match
         scheme/math
         (planet schematics/numeric:1/matrix)
         (planet schematics/numeric:1/vector)
         "point.ss"
         "pose.ss"
         "util.ss")

;; Cartesian Cartesian Cartesian -> (values Cartesian Number)
(define (line-segment-closest-point pt1 pt2 pt)
  (if (equal? pt1 pt2)
      (values pt1 (cartesian-distance pt1 pt))
      (let ()
        (match-define (struct cartesian [x1 y1]) pt1)
        (match-define (struct cartesian [x2 y2]) pt2)
        (match-define (struct cartesian [x y]) pt)

        (define u
          (/ (+ (* (- x x1) (- x2 x1))
                (* (- y y1) (- y2 y1)))
             (square (cartesian-distance pt2 pt1))))
        (define closest-pt
          (cond
           [(<= u 0) pt1]
           [(<= 1 u) pt2]
           [else (make-cartesian (+ x1 (* u (- x2 x1)))
                                 (+ y1 (* u (- y2 y1))))]))
        (define dist
          (cartesian-distance closest-pt pt))
        (values closest-pt dist))))

;; Cartesian Cartesian Cartesian Cartesian -> Cartesian
(define (line-line-intersection pt1 pt2 pt3 pt4)
  (match-define (struct cartesian [x1 y1]) pt1)
  (match-define (struct cartesian [x2 y2]) pt2)
  (match-define (struct cartesian [x3 y3]) pt3)
  (match-define (struct cartesian [x4 y4]) pt4)

  ;; From Wikipedia:
  ;;
  ;; P(x,y)= \bigg(&\frac{(x_1 y_2-y_1 x_2)(x_3-x_4)-(x_1-x_2)(x_3 y_4-y_3 x_4)}{(x_1-x_2)(y_3-y_4)-(y_1-y_2)(x_3-x_4)}, \\
  ;;        &\frac{(x_1 y_2-y_1 x_2)(y_3-y_4)-(y_1-y_2)(x_3 y_4-y_3 x_4)}{(x_1-x_2)(y_3-y_4)-(y_1-y_2)(x_3-x_4)}\bigg)
  (define x
    (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
          (* (- x1 x2) (- (* x3 y4) (* y3 x4))))
       (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))))
  (define y
    (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
          (* (- y1 y2) (- (* x3 y4) (* y3 x4))))
       (- (* (- x1 x2) (- y3 y4))
          (* (- y1 y2) (- x3 x4)))))
  (make-cartesian x y))


;; Polar Pose Pose -> Polar
;;
;; Project from ref-pose to new-pose
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
;; Project from ref-pose to new-pose
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


;; Number Number Number Number Number Number -> (Vectorof Cartesian)
;;
;; Constructs points evenly spaced at step radians around an ellipse.
;; xc and yc are the center coordinates
;; a and b are the semimajor and semiminor axis respectively
;; phi is the angle the semimajor axis makes to the x axis
(define (make-ellipse-points xc yc a b phi step)
  (list->vector
   (for/list ([t (in-range 0 (* 2 pi) step)])
     (make-cartesian (+ xc
                        (* a (cos t) (cos phi))
                        (- (* b (sin t) (sin phi))))
                     (+ yc
                        (* a (cos t) (sin phi))
                        (* b (sin t) (cos phi)))))))



(provide line-segment-closest-point
         line-line-intersection

         project-point
         project-points

         make-ellipse-points)