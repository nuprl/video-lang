#lang racket

(require racket/runtime-path
         video/base
         video/render
         video/render/png
         compiler/find-exe)

;; Opperate in same directory as script
(define-runtime-path here ".")
(define-runtime-path target "../rconframes")
(current-directory here)
;(define-runtime-path racket (find-exe))
;(define raco-video (curry system* racket "-l" "raco" "video"))

(render (clip "00005.MTS")
        target
        #:dest-filename "clip"
        #:render-mixin render-mixin
        #:speed 500
        #:start 1000
        #:end 30000)

(render (playlist (image "logo.png" #:length 10000)
                  (fade-transition #:length 3000)
                  (clip "00005.MTS"))
        target
        #:dest-filename "trans"
        #:render-mixin render-mixin
        #:speed 1000
        #:end 30000)

(render (multitrack (clip "stephen.MP4")
                    (composite-transition 0 0 1/4 1/4)
                    (clip "00005.MTS"))
        target
        #:dest-filename "pip"
        #:render-mixin render-mixin
        #:speed 1000
        #:end 30000)

(render (multitrack (color "white")
                    (composite-transition 1/4 0 3/4 3/4)
                    (clip "stephen.MP4")
                    (composite-transition 0 0 1/4 1/4)
                    (clip "00005.MTS"))
        target
        #:dest-filename "npip"
        #:render-mixin render-mixin
        #:speed 1000
        #:end 30000)

(render (multitrack (color "white")
                    (composite-transition 1/4 0 3/4 3/4)
                    (clip "stephen.MP4")
                    (composite-transition 0 0 1/4 1/4)
                    (clip "00005.MTS")
                    (composite-transition 0 1/2 1/4 1/4)
                    (image "logo.png"))
        target
        #:dest-filename "nlpip"
        #:render-mixin render-mixin
        #:speed 1000
        #:end 30000)
