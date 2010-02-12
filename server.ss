#lang scheme/base

(require
 scheme/foreign
 scheme/runtime-path
 scheme/tcp

 "point.ss"
 "pose.ss"
 "geometry.ss"
 (only-in "scan-match.ss" rotation))

(unsafe!)

(define-runtime-path here ".")

(define libidcserver (ffi-lib (build-path here "server/libidcserver")))

(define stop
  (get-ffi-obj
   "stop"
   libidcserver
   (_fun -> _void)))

(define (scan-match-error ref-pts ref-pose new-pts new-pose)
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

  (define proj-pts (project-points ref-pts ref-pose new-pose))
  (define-values (in out) (tcp-connect "localhost" 3478))
  
  (when (not (= (vector-length proj-pts) (vector-length new-pts)))
    (raise-mismatch-error
     'idc
     "Reference and new points do not have same length: "
     (cons (vector-length proj-pts) (vector-length new-pts))))

  (file-stream-buffer-mode out 'line)
  (display "OH HAI\n" out)
  (display "NIZ\n" out)
  (display (vector-length proj-pts) out)(newline out)
  (display-points proj-pts out)
  (display-points new-pts out)
  (display-double 0.0 out)
  (display-double 0.0 out)
  (display-double 0.0 out)
  (display-double rotation out)
  (display "KTHX\n" out)

  (read-line in)
  (let* ([err (read-double in)])
    (read-line in)
    (display "KTHXBAI\n" out)
    (read-line in)
    (close-output-port out)
    (close-input-port in)
    err))
  

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
 scan-match-error)