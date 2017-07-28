#lang scribble/acmart @acmsmall @screen @; @anonymous @review

@require["bib.rkt" (only-in scribble/core element make-style)
         "utils.rkt"]

@title{Super 8 Languages for Making Movies (Functional Pearl)}
@;@subtitle{(A Functional Pearl)}

@author[@list{Leif Andersen, Stephen Chang, Matthias Felleisen}
        #:affiliation (affiliation
                       #:institution
                       (institution
                        #:departments '("PLT" "CCIS")
                        "PLT @ Northeastern University")
                       #:city "Boston"
                       #:state "Mass."
                       #:country "United States of America")
	#:email "leif@ccs.neu.edu"]

@setcopyright{rightsretained}
@acmPrice{}
@acmDOI{10.1145/3110274}
@acmYear{2017}
@copyrightyear{2017}
@acmJournal{PACMPL}
@acmVolume{1}
@acmNumber{ICFP}
@acmArticle{30}
@acmMonth{9}

@abstract{The Racket doctrine tells developers to create
 languages (as libraries) to narrow the gap between the
 terminology of a problem domain and general programming
 constructs. This pearl illustrates this doctrine with the
 creation of a relatively simple domain-specific language for
 editing videos. To produce the video proceedings of a
 conference, for example, video professionals traditionally
 use ``non-linear'' GUI editors to manually edit each talk,
 despite the repetitive nature of the process. As it turns
 out, the task of video editing naturally splits into a
 declarative phase and an imperative rendering phase at the
 end. Hence it is natural to create a functional-declarative
 language for the first phase, which reduces a lot of manual
 labor. The implementation of this user-facing DSL, dubbed
 Video, utilizes a second, internal DSL to implement the
 second phase, which is an interface to a general, low-level
 C library. Finally, we inject type checking into our Video
 language via another DSL that supports programming in the
 language of type formalisms. In short, the development of
 the video editing language cleanly demonstrates how the
 Racket doctrine naturally leads to the creation of
 language hierarchies, analogous to the hierarchies of
 modules found in conventional functional languages.}

@keywords{Domain-Specific Language, Declarative Languages,
 Video Editing, Syntax Elaboration, Language Oriented Design,
 Movies, Integrated Development Environment}

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

@section[#:style 'unnumbered]{Acknowledgments}

We thank Asumu Takikawa for inspiring the idea of a Video language, Ben
Greenman for helping design the type system, and Benjamin Chung for bravely
undertaking the role of Video user #1. Thank you to our reviewers for their
detailed and insightful feedback. Finally, thank you to the MLT Framework
developers for their quick responses to our bug reports.

This work was supported by the US National
Science Foundation (SHF 1421412, SHF 1518844).

@gen-bib[#:sec-title "References"]
