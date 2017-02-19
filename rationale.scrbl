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

The Racket doctrine@cite[manifesto] says, developers must feel empowered
to easily create and deploy a new language to solve problems. Racket's
distinctive feature is a modular syntax system@cite[macros-icfp], which
blurs the distinction between languages and libraries. It is thus as easy
to import a linguistic construct as a function. While a library module may
provide functions that, say, solve problems in a mathematical domain, a
language module provides the basic services of a programming language.

In Racket, every module starts with a one-line language specification. For
example, @tt{#lang racket/base} tells the reader that the module is written
in the @tt{racket/base} language; the specification is pronounced ``hash
lang racket base.'' Roughly speaking, the specified language is the first
import into the module. From an implementation perspective, the language
specification points to a file that provides a language, approximately
speaking, a suite of linguistic features and run-time functions. A
developer can thus edit a language @tt{L} in one tab of an IDE and an
@tt{L} program in a second one. Any change to the first tab is immediately
visible in the second one, just by saving a file and switching tabs. Hence
language development in Racket suffers from no points of friction.

Due to the ease of developing and installing languages in the Racket
eco-system, language creation has become a critical arrow@margin-note*{We
considered ``war head'' but decided against it to appease the peacnicks on
the PC.} in the quiver of problem solving tools. When developers realize
that it is best to express themselves in the language of a domain, they do
not hesitate to develop a Racket dialect so that they can articulate their
solutions in this language programmatically. After all, domain experts have
developed this specialized jargon so that they can discuss problems and
solutions efficiently.

Given a base language, a Racket developer usually creates a language with
some or all of the following actions: 
@itemlist[
@item{adding new linguistic constructs;}

@item{hiding linguistic constructs;}

@item{re-interpreting linguistic constructs;}

@item{supplying run-time functions.}
]
Two of the most popular constructs for re-interpretation are function
application and the elements in a lexical scope (module, function). For
Video, the latter plays a critical role; otherwise, it is just a couple of 
thousand lines of veneer around a C library---the usual approach of creating
DSLs@cite[fowler]. Before we can describe Video and its implementation,
however, we need to survey the world of editing videos. 
