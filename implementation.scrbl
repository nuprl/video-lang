#lang scribble/sigplan

@require[(except-in scribble/manual cite)
         scriblib/figure
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "implementation"]{The Fast and the Furious}

Using the Racket ecosystem and design allows developers to
implement languages quickly and easily. Furthermore, these
languages compose, meaning that implementing a language like
Video is as simple as implementing video-specific
components, and then integrating them into the existing
Racket ecosystem.

This section discusses how a developer can implement a DSL
following the Racket design (@secref["impl-create"]). It
shows that going from a design to a prototype, to a full
working implementation requires little effort. Video serves
as an example of a DSL that follows this design.
@Secref["impl-video"] shows how Video goes from a design to
a full implementation. Finally, not only is Video
implemented using the Racket doctrine, but the
implementation of Video is implemented using the Racket
doctrine. That is to say, Video is implemented in a DSL
designed specifically for implementing Video, which itself
is implemented in Racket (@secref{impl-ffi}).

@section[#:tag "impl-create"]{Creating Languages - The Racket Way}

@section[#:tag "impl-video"]{Video Editing as a Language}

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

@section[#:tag "impl-ffi"]{Video - Behind the Scenes}
