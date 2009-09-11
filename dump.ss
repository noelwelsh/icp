#lang scheme/base

;; Dump scans to C parsable files

(require
 scheme/match
 "point.ss"
 "geometry.ss")


;; (Listof (Vectorof Polar)) -> String
;;
;; Scans should all share a common coordinate system for this to be sensible
(define (scans->c scans)
  (define n-scans (length scans))
  (define n-pts (vector-length (car scans)))
  (define out (open-output-string))
  (display "#include \"point.h\"\n" out)
  (fprintf out "polar_t scans[~a][~a];\n" n-scans n-pts)

  (display "void init_scans() {\n" out)
  (for ([i (in-naturals)]
        [pts (in-list scans)])
       (for ([j (in-naturals)]
             [pt (in-vector pts)])
            (fprintf out "scans[~a][~a].r = ~a; scans[~a][~a].a = ~a;\n"
                     i j (polar-r pt) i j (polar-a pt))))
  (display "}\n" out)
  (get-output-string out))
                     

(define (dump-scans file-name scans poses)
  ;; Transform all scans to the coordinate system of the first pose
  (define transformed-scans
    (for/list ([pts (in-vector scans)]
               [pose (in-vector poses)])
              (project-points pts pose (vector-ref poses 0))))
  
  (with-output-to-file file-name
    (lambda ()
      (display (scans->c transformed-scans)))
    #:exists 'replace))

;; Test
(require "austin-laser-points.ss")
(dump-scans "dump.c" points poses)


(provide
 scans->c
 dump-scans)
      