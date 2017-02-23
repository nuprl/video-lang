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

(define t#
  (case-lambda
    [(n) (t# "clip" n)]
    [(t n)
     (clip-frame (bitmap (build-path "rconframes"
                                     (format "~a~a.png" t (~a n
                                                              #:left-pad-string "0"
                                                              #:min-width 5
                                                              #:max-width 5
                                                              #:align 'right)))))]))

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
   (t# "nlpip" 3)
   (ellipses)
   (t# "nlpip" 6)
   (ellipses)
   (t# "nlpip" 9)
   (ellipses)
   (t# "nlpip" 12)
   (ellipses)
   (t# "nlpip" 15)
   (ellipses)
   (t# "nlpip" 18)
   (ellipses)
   (clip-scale (bitmap "res/alexis.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/rcon.png"))))

(define talk-list
  (if (directory-exists? "rconframes")
      (sort
       (append*
        (for/list ([f (directory-list "rconframes"
                                      #:build? #t)])
          (if (equal? (path-get-extension f) #".jpg")
              (list f)
              (list))))
       <=
       #:key (λ (key)
               (string->number (car (regexp-match #rx"[0-9]+"
                                                  (path->string key))))))
  (make-list 10000 "missing-file.png")))


(define elided
  (text "⟨⋯elided⋯⟩" text-font small-font-size))

(define splash
  (clip-frame (bitmap "res/rcon.png")))

(define splash2
  (clip-frame (vc-append
               (hc-append (bitmap "res/racket.png")
                          (bitmap "res/racket.png")
                          (bitmap "res/racket.png"))
               (hc-append (bitmap "res/racket.png")
                          (bitmap "res/racket.png")
                          (bitmap "res/racket.png")))))
