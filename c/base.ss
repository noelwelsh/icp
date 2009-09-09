#lang scheme/base

(require
 (for-syntax scheme/base)
 scheme/foreign
 scheme/runtime-path)

(unsafe!)

(define-runtime-path here ".")

(define libicp (ffi-lib (build-path here "libicp")))

;; (define-icp (scheme-name c-name type ...))
;;
;; Define a function exported by libicp
(define-syntax (define-icp stx)
  (syntax-case* stx () free-identifier=?
    [(define-icp (scheme-name c-name type ...))
     #'(define scheme-name
         (get-ffi-obj
          c-name
          libicp
          (_fun type ...)))]))


(provide
 libicp
 define-icp)