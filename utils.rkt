#lang racket

(provide (all-defined-out))
(require (except-in scribble/core table)
         racket/draw
         pict
         pict/code)

(define (exact . items)
  (make-element (make-style "identity" '(exact-chars))
                items))

(define (m . items)
  (make-element (make-style "identity" '(exact-chars))
                `("$" ,@items "$")))

(define (mm . items)
  (make-element (make-style "identity" '(exact-chars))
                `("\\[" ,@items "\\]")))

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
(define small-font-size 8)
(define font-size 9)
(define small-scale-factor 0.8)

(define-syntax-rule (mod->pict modname lang content ...)
  (let ()
    (define buffer 10)
    (define c1
      (scale (vl-append 2
                        (hbl-append (colorize ((current-code-tt) "#lang ") (current-keyword-color))
                                    (colorize ((current-code-tt) lang) (current-id-color)))
                        (code content ...))
             0.75))
    (define title (text modname null 7))
    (vl-append
     (cc-superimpose title (rectangle (+ buffer (pict-width title))
                                      (+ buffer (pict-height title))
                                      #:border-color "dim gray"))
     (cc-superimpose c1 (rectangle (+ buffer (pict-width c1))
                                   (+ buffer (pict-height c1))
                                   #:border-color "dim gray")))))
