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

@abstract{Conference videos combine several feeds into a single coherent
movie. Producing such videos usually employs a lot of repetitive work with
so-called non-linear editors. Fortunately, the domain of non-linear video
editing naturally allows the injection of a functional-declarative
scripting language with which it is possible to reduce a lot of the
repetitive work. 

This pearl presents the design of a video production language as an
illustration of the Racket doctrine, which says that languages belong in
the arsenal of software developers in the same way as type classes and
functors. Additionally, the pearl explains how Racket's eco-system
facilitates the addition of a type system. Finally, the pearl includes a
brief evaluation of the usefulness of the language.}

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

