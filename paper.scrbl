#lang scribble/acmart @format["acmlarge"] @; @anonymous @review

@require["bib.rkt" (only-in scribble/core element make-style)
         "utils.rkt"]

@title{Super 8, the Story of Making Movies---A Functional Pearl}
@authorinfo["J.J. Abrams"       "Bad Robot Productions"]
@authorinfo["Bryan Burk"        "Amblin Entertainment"]
@authorinfo["Steven Spielberg"  ""]

@;authorinfo["Leif Andersen" "PLT @ Northeastern University" "leif@ccs.neu.edu"]
@;authorinfo["Asumu Takikawa" "Igalia" "asumu@igalia.com"]
@;authorinfo["Stephen Chang" "PLT @ Northeastern University" "stchang@ccs.neu.edu"]
@;authorinfo["Matthias Felleisen" "PLT @ Northeastern University" "matthias@ccs.neu.edu"]

@; I liked the inverted structure.  (I was going to suggest something similar)
@abstract{The Racket doctrine tells developers to narrow the gap between
the language of a problem domain and the language of programming---every
time they encounter such a gap---by creating languages instead of just plain programs. This pearl illustrates this point with the
creation of a relatively simple domain-specific language for editing
videos. To produce a conference's video proceedings, for example, video professionals traditionally use "non-linear" GUI editors to manually edit each talk, despite similarities that may exist among the recordings. As it turns out, video
editing naturally splits the work into a declarative phase and an
imperative step at the end. Hence it is natural to create a
functional-declarative language for the first phase, which reduces a lot of
manual labor. Further, Racket programmers often support the creation of such user-facing DSLs with additional internal DSLs. For example, we wrap the second phase using a DSL that abstracts boilerplace and adds checks to improve the robustness of the low-level functions, and inject type checking into our language via another DSL that supports programming in the language of type formalisms.
In short, the development of the video editing language cleanly
demonstrates how the Racket doctrine naturally leads to the creation of
language hierarchies, analogous to the hierarchies of modules found in
conventional functional languages.}

@include-section{introduction.scrbl}

@;{Explain the Racket doctrine}
@include-section{rationale.scrbl}

@;{The background of how Video is constructed. Including two parts:
 A: Existing related DSLs (scribble, slideshow, etc.)
 B: Existing APIs for editing videos.}
@include-section{background.scrbl}

@;{Overview of the API for Video and how it becomes a language.}
@include-section{overview.scrbl}

@;{Describe (at a high level) the implementation of Video.}
@include-section{implementation.scrbl}

@;{Describe (at a high level) the type system of Video.}
@include-section{types.scrbl}

@;{WYSIWYG video editors are useful, and can compose with
 a video editing DSL.}
@include-section{extensions.scrbl}

@;{Cover related work not in background section}
@include-section{related.scrbl}

@;{Conclusions, not much to say here?}
@include-section{conclusion.scrbl}

@gen-bib[]

