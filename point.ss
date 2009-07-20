#lang scheme/base

(require (planet schematics/numeric:1/vector))

;; type point : (vector number number)

;; x, y, and angle
(define (make-point x y a)
  (vector x y a))

(define (point-x p)
  (vector-ref p 0))

(define (point-y p)
  (vector-ref p 1))

(define (point-a p)
  (vector-ref p 2))

(define (point-distance p1 p2)
  (sqrt
   (+ (square (- (point-x p1) (point-x p2)))
      (square (- (point-y p1) (point-y p2))))))  

(define (point+ p1 p2)
  (make-point
   (+ (point-x p1)
      (point-x p2))
   (+ (point-y p1)
      (point-y p2))))

(define (point/s p s)
  (make-point (/ (point-x p) s)
              (/ (point-y p) s)))

;; ((vectorof point) -> point)
(define (points-sum p)
  (for/fold ([sum (vector 0 0)])
      ([x (in-vector p)])
    (point+ x sum)))

(define (points-mean p)
  (point/s (points-sum p) (vector-length p)))

(define (point-copy p)
  (make-point (point-x p) (point-y p)))


(provide make-point
         point-x
         point-y
         point-a
         
         point-copy

         point-distance
         
         point+
         point/s
         
         points-sum
         points-mean)