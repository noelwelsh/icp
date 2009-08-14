#lang scheme/base

;; struct pose : Number Number Number
;;
;; Pose in 2-D: x, y, and angle
(define-struct pose (x y a) #:transparent)

(provide
 (struct-out pose))