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

@abstract{The Racket doctrine tells developers to narrow the gap between
the language of the problem domain and the language of programming---every
time they encounter such a gap. This pearl illustrates this point with the
creation of a relatively simple domain-specific language for editing
videos. Video professionals traditionally use non-linear editor, GUI tools
that require a lot of repetitive manual work for the production of a series
of similar recordings, say those of a conference. As it turns out, video
editing naturally splits the work into a declarative phase and an
imperative step at the end. Hence it is natural to create a
functional-declarative language for the first phase, which reduces a lot of
manual labor. For the injection of types into this scripting language,
Racket once again reduces a language gap, namely, the one between the
domain experts designing type-checking rules and the implementation
language. In short, the development of the video editing language cleanly
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

