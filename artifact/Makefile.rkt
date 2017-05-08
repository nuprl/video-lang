#lang racket
(require racket/runtime-path
         racket/cmdline)

(define git (find-executable-path "git"))
(define packer (find-executable-path "packer"))
(define 64bit #t)
(command-line
 #:program "artifact-builder"
 #:once-each
 [("--32-bit") "Compile a 32 bit image"
               (set! 64bit #f)])

(define-runtime-path here ".")
(parameterize ([current-directory (build-path here ".." "racket-video")])
  (git "archive" "--prefix=video" "-o" "video.tar" "master")
  (rename-file-or-directory "video.tar" here #t))
(parameterize ([current-directory (build-path here ".." "typed-video")])
  (git "archive" "--prefix=video" "-o" "typed-video.tar" "master")
  (rename-file-or-directory "typed-video.tar" here #t))
(if 64bit
    (packer "build" "artifact.json")
    (packer "build" "artifact32.json"))

#|
readme:
scribble +m --htmls README.scrbl
|#