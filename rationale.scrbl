#lang scribble/sigplan

@require[scriblib/figure
         pict
         pict/code
         racket/format
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@(current-code-font code-font)

@title[#:tag "rationale"]{The Design Rationale: Effects via Compilation}

Video demonstrates that an eDSL is often more than new function calls and
binding expressions.  A convenient-to-use eDSL may need a context-sensitive
traversal of independent phrases on top of a ``fuzzy'' interface to its
host language. 

Video is a descriptive, functional language. Most of its phrases describe
video clips. The language gathers the descriptions, and a video renderer
eventually presents the resulting video in a window. In between, functions
may process these video representations at will.  Conveniently, Video
allows inter-mingling function and variable definitions so that video
artists can place these elements near where they are used.

Video thus abstracts over functional or imperative patterns in Racket. A
functional pattern would require the Video programmer to thread variables
through series of expressions (say via @tt{let*}); the expressions
would also spell out how to gather the video-clip descriptions into a
single piece of data.  An imperative pattern would require the Video
programmer to mutate a variable (or several) to collect the video-clip
descriptions manually, with all the usual dangers that imperative
programming comes with.

Video avoids all of these dangers and remains declarative and
functional. Like a functional programmer, a Video programmer can manipulate
the code using the full power of functional calculi.Like a purely
functional language, Video compiles programs into imperative, low-level
object code. 

At first glance, Video is a domain-specific language algebraic effects for
video composition. It shifts the effect computations (concerning the
creation of a single, coherent video-clip) to compile time. Thus a Video
programmer can completely ignore that the video-clip descriptions are
manipulated in an imperative way before they get readied for the renderer. 

Need to compare with algebraic-effect languages 
