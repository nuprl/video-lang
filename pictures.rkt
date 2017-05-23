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


(define (language-tower s)
  (let ()
    (define (t str) (text str text-font font-size))
    (define racket (cc-superimpose (rectangle 370 30) (t "Racket")))
    (define syntax-parse (cc-superimpose (rectangle 170 30) (t "Syntax Parse")))
    (define scribble (cc-superimpose (rectangle 120 30) (t "Scribble")))
    (define video-ffi (cc-superimpose (rectangle 90 30) (t "Video FFI")))
    (define video (cc-superimpose (rectangle 90 30) (t "Video")))
    (define turnstyle (cc-superimpose (rectangle 90 30) (t "Turnstile")))
    (define typed-video (cc-superimpose (rectangle 90 30) (t "Typed Video")))
    (define video-doc (cc-superimpose (rectangle 90 30) (t "Video Docs")))
    (define offset
      (λ (rt-find delta-w)
        (λ (a b)
          (let-values ([(w h) (rt-find a b)])
            (values (- w delta-w) h)))))
    (define builds-on (t "builds on"))
    (let* ([acc (vc-append 50
                           (hc-append 70 video typed-video)
                           (hc-append 70 video-ffi turnstyle))]
           [acc (vc-append 50
                           acc
                           syntax-parse)]
           [acc* (vc-append 130
                            video-doc
                            scribble)]
           [acc (hc-append 60 acc* acc)]
           [acc (vc-append 50 acc racket)]
           [acc (pin-arrow-line 8 acc syntax-parse cb-find racket (offset ct-find -90)
                                #:x-adjust-label -40 #:label builds-on)]
           [acc (pin-arrow-line 8 acc scribble cb-find racket (offset ct-find 155)
                                #:x-adjust-label 25 #:label builds-on)]
           [acc (pin-arrow-line 8 acc video-ffi cb-find syntax-parse (offset lt-find -5)
                                #:x-adjust-label 25 #:label builds-on)]
           [acc (pin-arrow-line 8 acc turnstyle cb-find syntax-parse (offset rt-find 5)
                                #:x-adjust-label -40 #:label builds-on)]
           [acc (pin-arrow-line 8 acc video-ffi lc-find racket (offset ct-find 50)
                                #:x-adjust-label 15 #:label builds-on
                                #:start-angle pi #:end-angle (* pi -1/2) #:start-pull 3/5 #:end-pull 1/5)]
           [acc (pin-arrow-line 8 acc turnstyle (offset rb-find 20) racket (offset rt-find 50)
                                #:x-adjust-label -25 #:label builds-on
                                #:y-adjust-label 40
                                #:start-angle (* pi -1/4) #:end-angle (* pi 21/18) #:start-pull 1/3 #:end-pull 2/3)]
           [acc (pin-arrow-line 8 acc video cb-find video-ffi ct-find
                                #:x-adjust-label 25 #:label builds-on)]
           [acc (pin-arrow-line 8 acc typed-video cb-find turnstyle ct-find
                                #:x-adjust-label -40 #:label builds-on)]
           [acc (pin-arrow-line 8 acc video-doc cb-find scribble ct-find
                                #:x-adjust-label 25 #:label builds-on)]
           [acc (pin-arrow-line 8 acc typed-video lc-find video rc-find
                                #:label (t "extends"))]
           [acc (pin-arrow-line 8 acc video-doc rc-find video lc-find
                                #:label (t "extends"))]
           #;[acc (inset acc 100 0)])
      (scale acc s))))

;(language-tower 2)
