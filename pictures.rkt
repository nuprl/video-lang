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
  (define (t str (delta 0)) (text str text-font (- font-size delta)))
  (define *no 0)
  (define (L w h str)
    (set! *no (+ *no 1))
    (define language-no (t (number->string *no) 2))
    (define language-circle (cc-superimpose (ghost (rectangle 20 20)) (circle 16) language-no))
    (define language-box   (rectangle (+ w 20) h))
    (define language-label (t str))
    (rt-superimpose (cc-superimpose language-box language-label) language-circle))
  (define ((δ rt-find delta-w) a b)
    (define-values (w h) (rt-find a b))
    (values (- w delta-w) h))

  (define video     (L  90 30 "Video"))
  (define video-doc (L  90 30 "Video Docs"))
  (define t-video   (L  90 30 "Typed Video"))
  (define turnstyle (L  90 30 "Turnstile"))
  (define video-ffi (L  90 30 "Video FFI"))
  (define scribble  (L 120 30 "Scribble"))
  (define syn-parse (L 170 30 "Syntax Parse"))
  (define racket    (L 370 30 "Racket"))

  (define builds-on (t "builds on"))
  (define extends   (t "extends"))
  
  (let* ([acc (vc-append 50
                         (hc-append 60 (blank 20)
                                    (vc-append 130
                                               video-doc
                                               scribble)
                                    (vc-append 50
                                               (vc-append 50
                                                          (hc-append 70 video t-video)
                                                          (hc-append 70 video-ffi turnstyle))
                                               syn-parse) (blank 20))
                         racket)]
         [pin (λ (l1 find1 l2 find2 label (x-adjust 0) (sa #f) (ea #f) (sp 1/4) (ep 1/4))
                (set! acc
                      (pin-arrow-line 8 acc l1 find1 l2 find2
                                      #:x-adjust-label x-adjust #:label label
                                      #:start-angle sa #:end-angle ea #:start-pull sp #:end-pull ep))
                acc)]
         
         [acc (pin syn-parse cb-find racket    (δ ct-find -90) builds-on -40)]
         [acc (pin video-ffi cb-find syn-parse (δ lt-find  -5) builds-on  25)]
         [acc (pin turnstyle cb-find syn-parse (δ rt-find   5) builds-on -40)]
         [acc (pin video-ffi lc-find racket    (δ ct-find  50) builds-on  15 pi (* pi -1/2) 3/5 3/5)]
         [acc (pin video     cb-find video-ffi ct-find         builds-on  25)]
         [acc (pin t-video   cb-find turnstyle ct-find         builds-on -40)]
         [acc (pin t-video   lc-find video     rc-find         extends)]
         [acc (pin video-doc rc-find video     lc-find         extends)]
         [acc (pin video-doc cb-find scribble  ct-find         builds-on 25)]
         [acc (pin scribble  cb-find racket    (δ ct-find 155) builds-on 25)]
         [δ-rb (δ rb-find 20)]
         [acc (pin turnstyle δ-rb  racket      (δ rt-find 50)  builds-on  52 (* pi -1/4) (* pi 7/6))])
    (scale acc s)))

(language-tower 1.)
