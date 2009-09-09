#lang scheme/base

(require
 scheme/foreign
 scheme/math
 scheme/match
 (planet schematics/numeric:1/vector)
 (planet schematics/schemeunit:3)
 "angle.ss"
 "c/base.ss")

(define-cstruct _polar
  ([r _double*]
   [a _double*]))

(define-cstruct _cartesian
  ([x _double*]
   [y _double*]))

;; The default predicates aren't that useful as they don't
;; return #t, returning a list instead on success
(define (my-polar? p)
  (if (polar? p) #t #f))
(define (my-cartesian? p)
  (if (cartesian? p) #t #f))

(define-icp (polar->cartesian
             "polar_to_cartesian"
             (p : _polar) (out : (_ptr o _cartesian)) -> _void -> out))

(define-icp (cartesian->polar
             "cartesian_to_polar"
             (p : _cartesian) (out : (_ptr o _polar)) -> _void -> out))

(define-icp (cartesian+
             "cartesian_add"
             _cartesian _cartesian (out : (_ptr o _cartesian)) -> _void -> out))

(define-icp (cartesian-
             "cartesian_minus"
             _cartesian _cartesian (out : (_ptr o _cartesian)) -> _void -> out))

(define-icp (cartesian=?
             "cartesian_equal"
             _cartesian _cartesian -> _bool))

(define-icp (cartesian-distance
             "cartesian_distance"
             _cartesian _cartesian -> _double))

(define-icp (cartesian-dot
             "cartesian_dot"
             _cartesian _cartesian -> _double))

;; cartesian-transform : Cartesian Number Number Number -> Cartesian
;;
;; Transform a point by a translation and a rotation. We
;; assume the rotation is about the origin, and is performed
;; before the translation
(define-icp (cartesian-transform
             "cartesian_transform"
             _cartesian _double* _double* _double* (out : (_ptr o _cartesian)) -> _void -> out))


(define-icp (polar+
             "polar_add"
             _polar _polar (out : (_ptr o _polar)) -> _void -> out))

(define-icp (polar-
             "polar_minus"
             _polar _polar (out : (_ptr o _polar)) -> _void -> out))

(define-icp (polar-dot
            "polar_dot"
            _polar _polar -> _double))

(define-icp (polar-rotate
             "polar_rotate"
             _polar _double* (out : (_ptr o _polar)) -> _void -> out))

(define-icp (polar-normalise
             "polar_normalise"
             _polar (out : (_ptr o _polar)) -> _void -> out))


(define-check (check-point p1 p2 e)
  (check-true (or (and (my-cartesian? p1)
                       (my-cartesian? p2))
                  (and (my-polar? p1)
                       (my-polar? p2)))
              (format "Points ~a and ~a are of different types" p1 p2))
  (if (cartesian? p1)
      (begin
        (check-= (cartesian-x p1) (cartesian-x p2) e
                 (format "Points ~a and ~a x coordinates differ by more than ~a"
                         p1 p2 e))
        (check-= (cartesian-y p1) (cartesian-y p2) e
                 (format "Points ~a and ~a y coordinates differ by more than ~a"
                         p1 p2 e)))
      (begin
        (check-= (polar-r p1) (polar-r p2) e
                 (format "Points ~a and ~a radii differ by more than ~a"
                         p1 p2 e))
        (check-= (polar-a p1) (polar-a p2) e
                 (format "Points ~a and ~a angles differ by more than ~a"
                         p1 p2 e)))))


(define (polar->vector p)
  (vector (polar-r p) (polar-a p)))
(define-match-expander polar
  (syntax-rules ()
    [(polar (r a))
     (app polar->vector (vector r a))]))
(define (cartesian->vector p)
  (vector (cartesian-x p) (cartesian-y p)))
(define-match-expander cartesian
  (syntax-rules ()
    [(polar (x y))
     (app cartesian->vector (vector x y))]))

(provide
 polar
 polar->vector
 make-polar
 (rename-out [my-polar? polar?])
 polar-r
 polar-a
 set-polar-r!
 set-polar-a!
 _polar
 _polar-pointer
 
 cartesian
 cartesian->vector
 make-cartesian
 (rename-out [my-cartesian? cartesian?])
 cartesian-x
 cartesian-y
 _cartesian
 
 polar->cartesian
 cartesian->polar

 cartesian+
 cartesian-
 cartesian=?
 cartesian-distance
 cartesian-dot
 cartesian-transform
 
 polar+
 polar-
 polar-dot
 polar-rotate
 polar-normalise

 check-point)
