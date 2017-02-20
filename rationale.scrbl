#lang scribble/sigplan

@require[scriblib/figure
         pict
         pict/code
         racket/format
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@(current-code-font code-font)

@title[#:tag "rationale"]{Dr Strangelove: How I Learned to Stop Worrying and Love the Racket}
@; ----------------------------------------------------------------------------------------

The Racket doctrine@cite[manifesto] says that developers must feel
empowered to easily create and deploy a new language to solve problems and
organize their software. Philosophically, Racket achieves this ideal with a
radical emphasis on linguistic reuse@cite[SK-PhD]. Technically, this reuse
is enabled via Racket's distinctive feature: a modular syntax
system@cite[macros-icfp]. This systemx blurs the distinction between
languages and libraries. It is thus as easy to import a linguistic
construct as a function. While a library module exports functions with a
related purpose, a language module provides the constructs of a programming
language.

In Racket, every module starts with a one-line language specification. For
example, the specification @tt{#lang racket/base}---pronounced ``hash lang
racket base''---tells Racket and a potential reader that the module is
written in the @tt{racket/base} language. Roughly speaking, the specified
language is the first import into the module. From an implementation
perspective, the language specification points to a file that provides a
language, approximately speaking, a suite of linguistic features and
run-time functions. A developer can thus edit a language @tt{L} in one
buffer of an IDE and an @tt{L} program in a second one. Any change to the
first is immediately visible in the second one, just by switching
focus. Hence language development in Racket suffers from no points of
friction.

Developing a new language typically starts from a base language, reusing as
many features as possible. From this language, a Racket developer usually
creates a language with some or all of the following actions:
@;
@itemlist[ 

@item{adding new linguistic constructs;}

@item{hiding linguistic constructs; and}

@item{re-interpreting linguistic constructs.}
]
For the latter, Racket developers exploit macros and also linguistic
interposition points, that is, anchors in the syntax elaboration process
where a program may inject additional syntax transformations. Video
critically relies on all of the above, plus of course, the ability to
supply additional run-time primitives with a new language

Due to the ease of developing and installing languages in the Racket
eco-system, language creation has become a critical arrow@margin-note*{We
considered ``war head'' but decided against it to appease the peacnicks on
the PC.} in the quiver of software-engineering tools, of comparable status
as Haskell's type classes and ML's functors. When developers realize that
it is best to express themselves in the language of a domain, they do not
hesitate to develop matching programming language. After all, domain
experts have developed this specialized jargon so that they can discuss
problems and solutions efficiently. 

The domain of video editing is a particularly well-suited domain for
illustrating the above points. While the evolution of the language follows
the standard path from a veneer for a C library to a full-fledged
language@cite[fowler], Racke smoothens this path significantly and this
pearl demonstrates how.  Before we can describe Video and its
implementation, however, we need to survey the world of editing videos.
