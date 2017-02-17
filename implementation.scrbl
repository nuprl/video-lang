#lang scribble/sigplan

@require[(except-in scribble/manual cite)
         scriblib/figure
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "implementation"]{Implementing Video}

Video's implementation is spread across several main
components: a surface syntax, a core library, and a
rendering layer. Racket's meta-programming
facilities@cite[macros-icfp] play a major role in the
implementation. This section discusses Video's
implementation, focusing on aspects of the Racket dogma that
facilitate the languages implementation.

Video is implemented in 3219 lines of code. Of that, about
95 lines are specific to the surface syntax, and 353 lines
define the video-specific primitives the language uses. The
video-specific primitives are implemented using standard
functional programming techniques. They only serve as a thin
wrapper of functions to Video's core data-types.

Surface syntax is where Video differs from embedded DSL that
are not implemented using the Racket design process.
Normally, the syntax for these languages are cobbled
together using either low-level parsing tools or by
modifying the implementation of an existing language. Here,
Video composes with the rest of the Racket ecosystem, while
still remaining a DSL.

