#lang at-exp racket

(require "../utils.rkt")

@(type-table `(blank (-> Number Producer))
             `(color (-> (U String (X Number Number Number)) Producer))
             `(clip (-> String Producer))
             `(image (-> String Producer))
             `(playlist (-> (List (U Producer (-> Producer Producer Producer)))
                            #\newline
                            #:transitions
                            (List (U/man (X (-> Producer Producer Producer) Producer Producer)
                                         #\newline
                                         (Ghost "\\texttt{\\#:transitions} [( ")
                                         (Const " \\mid ")
                                         (X (-> Producer Producer) Producer)))
                            #\newline
                            Producer))
             `(multitrack (-> (List (U Producer (-> Producer Producer Producer)))
                              #\newline
                              #:transitions
                              (List (U/man (X (-> Producer Producer Producer) Producer Producer)
                                           #\newline
                                           (Ghost "\\texttt{\\#:transitions} [( ")
                                           (Const " \\mid ")
                                           (X (-> Producer Producer) Producer)))
                              #\newline
                              Producer))
             `(get-property (-> Producer String Any))
             `(set-property (-> Producer String Any Producer))
             `(attach-filter (-> Producer (-> Producer Producer) Producer))
             `(grayscale-filter (-> Producer Producer))
             `(cut-filter (-> Number Number (-> Producer Producer)))
             `(scale-filter (-> Number Number (-> Producer Producer)))
             `(translate-filter (-> Number Number (-> Producer Producer)))
             `(fade-transition (-> Number (-> Producer Producer Producer)))
             `(swipe-transition (-> String Number (-> Producer Producer Producer)))
             `(composite-transition (-> Number Number Number Number
                                        #\newline
                                        (-> Producer Producer Producer))))
