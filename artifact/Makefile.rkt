#!/usr/bin/env racket
#lang racket
(require racket/runtime-path
         racket/cmdline
         file/tar)

(define git (find-executable-path "git"))
(define packer (find-executable-path "packer"))
(define 64bit #t)
(command-line
 #:program "artifact-builder"
 #:once-each
 [("--32-bit") "Compile a 32 bit image"
               (set! 64bit #f)])

(define-runtime-path here ".")
(current-directory here)
(parameterize ([current-directory (build-path here ".." "racket-video")])
  (system* git "archive" "--prefix=video" "-o" "video.tar" "master")
  (rename-file-or-directory "video.tar" (build-path here "video.tar") #t))
(parameterize ([current-directory (build-path here ".." "typed-video")])
  (system* git "archive" "--prefix=video" "-o" "typed-video.tar" "master")
  (rename-file-or-directory "typed-video.tar" (build-path here "typed-video.tar") #t))
(tar "icfp-2017-artifact.tar" "video.tar" "typed-video.tar"
     #:exists-ok? #t)
(if 64bit
    (system* packer "build" "artifact.json")
    (system* packer "build" "artifact32.json"))

#|
readme:
scribble +m --htmls README.scrbl
|#