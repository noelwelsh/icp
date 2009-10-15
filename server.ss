#lang scheme/base

(require
 scheme/foreign
 scheme/runtime-path
 scheme/tcp

 "point.ss")

(unsafe!)

(define-runtime-path here ".")

(define libidcserver (ffi-lib (build-path here "server/libidcserver")))

(define stop
  (get-ffi-obj
   "stop"
   libidcserver
   (_fun -> _void)))

(define (idc ref-pts new-pts xt yt a rotation)
  (define (read-double in)
    (floating-point-bytes->real (read-bytes 8 in) #f))
  (define (display-double n out)
    (write-bytes (real->floating-point-bytes n 8 #f) out))
  (define (display-polar pt out)
    (display-double (polar-r pt) out)
    (display-double (polar-a pt) out))
  (define (display-points pts out)
    (for ([pt (in-vector pts)])
         (display-polar pt out)))
  (define-values (in out) (tcp-connect "localhost" 3478))
  
  (when (not (= (vector-length ref-pts) (vector-length new-pts)))
    (raise-mismatch-error
     'idc
     "Reference and new points do not have same length: "
     (cons (vector-length ref-pts) (vector-length new-pts))))

  (file-stream-buffer-mode out 'line)
  (display "OH HAI\n" out)(display "saying hai\n")
  (display "NIZ\n" out)(display "saying niz\n")
  (display (vector-length ref-pts) out)(display "\n" out)
  (display-points ref-pts out)(display "sending points\n")
  (display-points new-pts out)
  (display-double xt out)
  (display-double yt out)
  (display-double a out)
  (display-double rotation out)
  (display "KTHX\n" out)(display "over and out\n")

  (read-line in)
  (let* ([xt (read-double in)]
         [yt (read-double in)]
         [a  (read-double in)])
    (read-line in)
    (display "KTHXBAI\n" out)
    (read-line in)
    (close-output-port out)
    (close-input-port in)
    (values xt yt a)))
  

(define (test)
  (define-values (in out) (tcp-connect "localhost" 3478))
  (display "OH HAI\n" out)
  (display "NIZ\n" out)
  (display 180 out)
  (close-output-port out)
  (close-input-port in))

(provide
 stop
 test
 idc)