#lang scribble/acmart @format["acmlarge"]

@require["bib.rkt"
         "utils.rkt"]

@title{A Declarative Embedded DSL for Video Editing}
@authorinfo["Leif Andersen" "PLT @ Northeastern University" "leif@ccs.neu.edu"]
@authorinfo["Asumu Takikawa" "Igalia" "asumu@igalia.com"]
@authorinfo["Matthias Felleisen" "PLT @ Northeastern University" "matthias@ccs.neu.edu"]


@abstract{Video is an Embedded DSL, and this is its paper's abstract.}

@;{Describe concrete example of editing conference videos.
 Describe how task is repetitive, and how a programmer would want to
 automate it.}
@include-section{introduction.scrbl}

@;{The background of how Video is constructed. Including two parts:
 A: Existing related DSLs (scribble, slideshow, etc.)
 B: Existing APIs for editing videos.}
@include-section{background.scrbl}

@;{Overview of the API for Video
 and how it becomes a language.}
@include-section{overview.scrbl}

@;{A Video DSL saves time and energy in in the concrete
 case of editing conference videos.}
@include-section{casestudy.scrbl}

@;{Describe (at a high level) the implementation
 of Video.}
@include-section{implementation.scrbl}

@;{WYSIWYG video editors are useful, and can compose with
 a video editing DSL.}
@include-section{extensions.scrbl}

@;{Cover related work not in background section}
@include-section{related.scrbl}

@;{Conclusions, not much to say here?}
@include-section{conclusion.scrbl}

@gen-bib[]

