#!/usr/bin/env racket
#lang racket

(require pkg
         pkg/lib
         racket/cmdline
         compiler/find-exe)

(define update-deps #f)

(command-line
 #:program "paper setup"
 #:once-each
 [("-u" "--update") "Update deps"
                    (set! update-deps #t)]
 #:args ()
 (void))

(define pkgs '("doodle"
               "video"))


(for ([i (in-list pkgs)])
  (cond [(and (hash-has-key? (installed-pkg-table) i))
         (when update-deps
           (pkg-update-command #:deps 'search-auto i #:no-setup #t)
           (system* (find-exe) "-l" "raco" "setup" "--no-docs" "video"))]
        [else
         (pkg-install-command #:deps 'search-auto i #:no-setup #t)
         (system* (find-exe) "-l" "raco" "setup" "--no-docs" "video")]))
