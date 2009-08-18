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


(provide line-segment-closest-point)