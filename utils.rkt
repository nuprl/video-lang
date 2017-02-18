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

(define (->text . text)
  (make-element (make-style "identity" '(exact-chars))
                `("\\begin{tikzpicture}"
                  "\\node (0) {};"
                  "\\node [left of=0, xshift=4cm] (1) {};"
                  "\\draw[->,transform canvas={yshift=-1mm}] (0) -- node[yshift=1mm]"
                  ,(format "{\\tiny ~a} (1);" (apply string-append text))
                  "\\end{tikzpicture}")))

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

(define matthias-suffix (if (regexp-match #px"matthias" (current-directory)) " O" ""))
(define code-font (string-append "Linux Libertine Mono" matthias-suffix))
(define text-font (string-append "Linux Libertine" matthias-suffix))
(define small-font-size 12)
(define font-size 15)
(define small-scale-factor 0.8)
(define code-line-sep 10)

; Test to make sure fonts are installed:
(unless (set-member? (get-face-list) code-font)
  (raise-user-error 'paper "Please install '~a' font: http://www.linuxlibertine.org" code-font))
(unless (set-member? (get-face-list) text-font)
  (raise-user-error 'paper "Please install '~a' font: http://www.linuxlibertine.org" text-font))

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

(define (make-playlist-timeline #:distance [distance 5]
                                #:end [end #f]
                                #:font-size [font-size small-font-size]
                                . trace)
  (define frames
   (apply hc-append distance trace))
  (vc-append
   15
   frames
   (let ([p (hc-append (pict-width frames)
                       (tag-pict (vline 1 10) 'start)
                       (tag-pict (if end (vline 1 10) (blank)) 'end))])
     (pin-arrow-line 5 p #:label (text "time" text-font font-size)
                     (find-tag p 'start) cc-find
                     (find-tag p 'end) cc-find))))

(define (ellipses #:offset [offset 3]
                  #:size [size 2])
  (hc-append
   offset
   (disk size)
   (disk size)
   (disk size)))

(define (clip-scale p)
  (scale-1080p p 30))

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

(define (split-minipage a b #:split-location [split-location 0.5]
                        #:direction [direction "c"])
  (centered
   (list
    @exact{\begin{minipage}[@direction]{@(number->string split-location)\textwidth}}
    a
    @exact{\end{minipage}\begin{minipage}[@direction]{@(number->string (- 1 split-location))\textwidth}}
    b
    @exact{\end{minipage}})))

(define (3split-minipage a b c
                         #:size-a [size-a 1/3]
                         #:size-b [size-b 1/3]
                         #:size-c [size-c 1/3]
                         #:direction [direction "c"])
  (centered
   (list
    @exact{\begin{minipage}[@direction]{@(number->string size-a)\textwidth}}
    a
    @exact{\end{minipage}\begin{minipage}[@direction]{@(number->string size-b)\textwidth}}
    b
    @exact{\end{minipage}\begin{minipage}[@direction]{@(number->string size-c)\textwidth}}
    c
    @exact{\end{minipage}})))

(define (minipage #:size [size 1] . a)
  (append (list @exact{\begin{minipage}{@(number->string size)\textwidth}})
          a
          (list @exact{\end{minipage}})))

(define (TODO . content)
  (elem #:style (style #f (list (color-property "red")))
        content))

(define (type-table . table)
  (make-element (make-style "identity" '(exact-chars))
                `("\\begin{align*}"
                  ,@(add-between
                     (for/list ([i (in-list table)])
                       (format "\\textit{~a} :&\\ ~a" (first i) (type->latex-str (second i))))
                     "\\\\")
                  "\\end{align*}")))

(define parenize (make-parameter #f))
(define (type->latex-str type)
  (match type
    [`(-> ,type* ... ,ret-type)
     (format (if (parenize) "(~a \\rightarrow ~a)" "~a \\rightarrow ~a")
             (parameterize ([parenize #t])
               (string-join (map type->latex-str type*)
                            "\\; "))
             (type->latex-str ret-type))]
    [`(U ,type* ...)
     (format (if (parenize) "(~a)" "~a")
             (parameterize ([parenize #t])
               (string-join (map type->latex-str type*)
                            " \\mid ")))]
    [`(U/man ,type* ...)
     (format (if (parenize) "(~a)" "~a")
             (parameterize ([parenize #t])
               (map type->latex-str type*)))]
    [`(X ,type* ...)
     (format (if (parenize) "(~a)" "~a")
             (parameterize ([parenize #t])
               (string-join (map type->latex-str type*)
                            " \\times ")))]
    [`(List ,type)
     (format "[~a\\: \\cdots]"
             (type->latex-str type))]
    [`(Ghost ,text)
     (format "\\phantom{~a}" text)]
    [`(Const ,text)
     text]
    [#\newline "\\\\ & "]
    [(? keyword?) (format "\\texttt{\\~a}" type)]
    [_ (format "\\mathsf{~a}" type)]))
