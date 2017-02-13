#lang at-exp racket

(require pict
         pict/color
         "utils.rkt")
(provide (all-defined-out))

(define hello-green
  (vc-append
   10
   (mod->pict "green.vid" "video" (clip "green"))
   (hline 200 0)
   (scale (bitmap "res/sample.png") 0.08)))

(define nlve-sample
  (scale (bitmap "res/nlve-demo.png") 0.14))

(define blank-rect
  (clip-scale (blank 1 1)))

(define circ-image
  (cc-superimpose blank-rect
                  (disk 10 #:color "yellow")))

(define rect-clip-frames 10)
(define rect-clip
  (build-list rect-clip-frames
              (λ (f#)
                (cc-superimpose blank-rect
                                (rotate (filled-rectangle 15 15 #:color "red")
                                        (/ (* f# pi) rect-clip-frames 2))))))

(define (clip-frame clip)
  (cc-superimpose (clip-scale (rectangle 50 50))
                  (clip-scale clip)))

(define rcon-timeline
  (make-playlist-timeline
   #:end #t
   (clip-scale (bitmap "res/rcon.png"))
   (ellipses)
   (clip-scale (bitmap "res/geoffrey.jpg"))
   (ellipses)
   (clip-scale (bitmap "rconframes/stephen50.jpg"))
   (ellipses)
   (clip-scale (bitmap "rconframes/stephen75.jpg"))
   (ellipses)
   (clip-scale (bitmap "rconframes/stephen80.jpg"))
   (ellipses)
   (clip-scale (bitmap "rconframes/stephen100.jpg"))
   (ellipses)
   (clip-scale (bitmap "rconframes/stephen250.jpg"))
   (ellipses)
   (clip-scale (bitmap "rconframes/stephen300.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/alexis.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/rcon.png"))))
