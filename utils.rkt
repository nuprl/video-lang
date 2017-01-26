#lang at-exp racket

(provide (all-defined-out))
(require (except-in scribble/core table)
         scribble/base
         racket/draw
         pict
         pict/code
         ppict/tag)

(define (exact . items)
  (make-element (make-style "identity" '(exact-chars))
                items))

(define (m . items)
  (make-element (make-style "identity" '(exact-chars))
                `("$" ,@items "$")))

(define (mm . items)
  (make-element (make-style "identity" '(exact-chars))
                `("\\[" ,@items "\\]")))

(define (paragraph title)
  (make-element (make-style "identity" '(exact-chars))
                `("\\paragraph{" ,title "}")))

(define at-char "@")

(define dot (find-executable-path "dot"))

(define (dot->pict . graph)
  (define-values (in out) (make-pipe))
  (with-input-from-string (apply string-append graph)
    (λ () (parameterize ([current-output-port out])
            (system* dot "-Tpng"))))
  (define b (make-object bitmap% 1 1))
  (send b load-file in)
  (bitmap b))

(define code-font "CMU Typewriter Text")
(define text-font "Times")
(define small-font-size 14)
(define font-size 15)
(define small-scale-factor 0.8)
(define code-line-sep 10)

; Test to make sure fonts are installed:
(unless (set-member? (get-face-list) code-font)
  (raise-user-error 'paper "Please install '~a' font" code-font))
(unless (set-member? (get-face-list) text-font)
  (raise-user-error 'paper "Please install '~a' font" text-font))

(define-syntax-rule (mod->pict modname lang content ...)
  (mod->pict* #:codeblock? #f modname lang content ...))

(define-syntax-rule (modblock->pict modname lang content ...)
  (mod->pict* #:codeblock? #t modname lang content ...))

(define-syntax-rule (mod->pict* #:codeblock? codeblock? modname lang content ...)
  (let ()
    (define buffer 10)
    (define c1
      (scale (vl-append 2
                        (hbl-append (colorize ((current-code-tt) "#lang ") (current-keyword-color))
                                    (colorize ((current-code-tt) lang) (current-id-color)))
                        (if codeblock?
                            (codeblock-pict
                             #:keep-lang-line? #f
                             (~a "#lang" lang "\n" content ...))
                            (code content ...)))
             0.75))
    (define title (text modname null 7))
    (vl-append
     (cc-superimpose title (rectangle (+ buffer (pict-width title))
                                      (+ buffer (pict-height title))
                                      #:border-color "dim gray"))
     (cc-superimpose c1 (rectangle (+ buffer (pict-width c1))
                                   (+ buffer (pict-height c1))
                                   #:border-color "dim gray")))))

(define (vsplit-figure a b #:space [space 25])
  (vc-append
   a
   (blank space)
   (linewidth 0
              (hline (max (pict-width a) (pict-width b)) 1))
   (blank space)
   b))

(define (make-playlist-timeline #:distance [distance 5] . trace)
  (define frames
   (apply hc-append distance trace))
  (vc-append
   15
   frames
   (let ([p (hc-append (pict-width frames) (tag-pict (blank) 'start) (tag-pict (blank) 'end))])
     (pin-arrow-line 5 p #:label (text "Time" text-font small-font-size)
                     (find-tag p 'start) cc-find
                     (find-tag p 'end) cc-find))))

(define (ellipses)
  (hc-append
   3
   (disk 2)
   (disk 2)
   (disk 2)))

(define (clip-scale p)
  (scale-1080p p 50))

(define (scale-1080p p w-size)
  (define w (pict-width p))
  (define h (pict-height p))
  (define h-size (* w-size 9/16))
  (scale p
         (/ w-size w)
         (/ h-size h)))

(define (code-pict code)
  (nested #:style (style 'code-inset '(never-indents))
          code))

(define (split-minipage a b #:split-location [split-location 0.5])
  (centered
   (list
    @exact{\begin{minipage}{@(number->string split-location)\textwidth}}
    a
    @exact{\end{minipage}\begin{minipage}{@(number->string (- 1 split-location))\textwidth}}
    b
    @exact{\end{minipage}})))

(define (minipage . a)
  (append (list @exact{\begin{minipage}{\textwidth}})
          a
          (list @exact{\end{minipage}})))
