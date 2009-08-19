#lang scheme/base

(require scheme/match
         "point.ss"
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

(provide line-segment-closest-point
         line-line-intersection)