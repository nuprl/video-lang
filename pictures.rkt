#lang at-exp racket

(require pict
         pict/color
         "utils.rkt")
(provide (all-defined-out))

(define hello-green
  (vc-append
   15
   (mod->pict "green.vid" "video" (clip "green"))
   (hline 200 0)
   (scale (bitmap "res/sample.png") 0.15)))

(define nlve-sample
  (scale (bitmap "res/nlve-demo.png") 0.14))
