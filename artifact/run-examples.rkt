#!/usr/bin/env racket
#lang racket

(require racket/runtime-path
         compiler/find-exe)

(define-runtime-path here ".")
(define rv (curry system* (find-exe) "-l" "raco" "video"))

(parameterize ([current-directory (build-path here "examples")])
  (rv "1-hellocolor.rkt")
  (rv "2-colorfade.rkt")
  (rv "3-clips.rkt")
  (rv "4-filters.rkt")
  (rv "5-project.rkt")
  (rv "6-multitrack.rkt")
  (rv "7-watermark.rkt")
  (rv "8-image.rkt")
  (rv "9-doubletransition.rkt")
  (rv "10-properties.rkt")
  (rv "11-include.rkt")
  (rv "12-cut.rkt"))
