#lang scheme/base

;; These are simple tests to visualise (and hence check) the point matching parts of ICP and IMRP, as well as some of the machinery around them

;; Not part of the main test suite as 1) it requires MrEd
;; and 2) it ain't automated

(require (planet schematics/numeric:1/bonfire/screen)
         (planet schematics/numeric:1/bonfire/frame)
         (planet schematics/numeric:1/vector)
         "geometry.ss"
         "point.ss"
         (prefix-in icp: "icp.ss")
         (prefix-in imrp: "imrp.ss"))

;; (U Cartesian Polar) -> Cartesian
(define (point->cartesian pt)
  (if (polar? pt)
      (polar->cartesian pt)
      pt))

;; Cartesian -> Vector
(define (cartesian->vector pt)
  (vector-immutable (cartesian-x pt) (cartesian-y pt)))

;; (Vectorof Cartesian) -> (Listof Vector)
(define (cartesian-points->vectors pts)
  (for/list ([p (in-vector pts)])
    (cartesian->vector (point->cartesian p))))

(define (points->dots pts)
  (dots (cartesian-points->vectors pts)))

;; Make sure the ellipses are being generated correctly
(define (plot-ellipses)
  (plot-screen
   "Ellipses"
   (overlays (list (colour (points->dots (make-ellipse-points 4 4 10 5 0 .2))
                           (vector-immutable 255 0 0))
                   (colour (points->dots (make-ellipse-points 0 0 10 5 0.4 .2))
                           (vector-immutable 0 255 0))
                   ;; This should be the same as the red ellipse above, so only one ellipse should render
                   (colour (points->dots (vector-map (lambda (pt) (cartesian-transform pt 2 2 -.1))
                                                     (make-ellipse-points 2 2 10 5 .1 .2)))
                     (vector-immutable 255 0 0))))))

;; (Vectorof Cartesian) (Vectorof Cartesian) -> Frame
(define (matches->lines pts matches)
  (overlays
   (for/list ((p (in-vector pts))
              (m (in-vector matches)))
     (line (cartesian->vector (point->cartesian p))
           (cartesian->vector (point->cartesian m))))))

;; Make sure matches are sensible
(define (plot-icp-matches)
  (define ref-pts (vector-map cartesian->polar (make-ellipse-points 3 3 40 20 .10 .1)))
  (define new-pts (vector-map cartesian->polar (make-ellipse-points 6 6 40 20 .15 .1)))
  (define matches (icp:matching-points new-pts ref-pts .2))
  (plot-screen
   "Matches"
   (overlays (list (colour (points->dots ref-pts) (vector-immutable 255 0 0))
                   (colour (points->dots new-pts) (vector-immutable 0 255 0))
                   (colour (matches->lines new-pts matches) (vector-immutable 128 128 128))))))

(define (plot-imrp-matches)
  (define ref-pts (vector-map cartesian->polar (make-ellipse-points 3 3 40 20 .10 .1)))
  (define new-pts (vector-map cartesian->polar (make-ellipse-points 6 6 40 20 .15 .1)))
  (define matches (imrp:matching-points new-pts ref-pts .2))
  (plot-screen
   "Matches"
   (overlays (list (colour (points->dots ref-pts) (vector-immutable 255 0 0))
                   (colour (points->dots new-pts) (vector-immutable 0 0 255))
                   (colour (matches->lines new-pts matches) (vector-immutable 128 128 128))))))
