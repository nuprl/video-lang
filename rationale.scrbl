#lang scribble/acmart

@require[scriblib/figure
         pict
         (except-in scribble/manual cite)
         (except-in pict/code code typeset-code)
         (prefix-in pict: pict/code)
         racket/format
         scriblib/footnote
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@(current-code-font code-font)

@title[#:tag "rationale"]{Dr Strangelove: How I Learned to Stop Worrying and Love Racket}
@; ----------------------------------------------------------------------------------------

The Racket doctrine@cite[manifesto] says that developers
must feel empowered to easily create and deploy a new
language when a software development situation demands it.
The emphasis on language reflects the Racket team's deep
embrace of the Sapir-Whorf hypothesis. In the world of
software, this hypothesis means that language frames the
developer's ability to understand a problem and articulate a
solution---at every stage in the development process.

Philosophically, Racket achieves this ideal with a radical emphasis on
linguistic reuse@cite[SK-PhD]. Technically, this reuse is enabled via
Racket's distinctive feature: a modular syntax system@cite[macros-icfp].
In this system, it is easy to import a linguistic construct as a
function; indeed the system blurs the distinction between languages and
libraries, e.g. @citet[lal-pldi]. While a library module exports functions with a
related purpose, a language module exports the constructs of a programming
language.

In Racket, every module imports its language with a one-line language specification. For
example, the line @code{#lang racket/base}---pronounced ``hash lang
racket base''---tells Racket and a future reader that the module is
written in the @racketmodname[racket/base] language. That is, the specified
language is really the first import into the module. From an implementation
perspective, the language specification points to a file that provides a
language, approximately speaking, a suite of linguistic features and
run-time functions. A developer can thus edit a language @tt{L} in one
buffer of an IDE and an @tt{L} program in a second one. Any change to the
first is immediately visible in the second one, just by switching
focus. Language development in Racket suffers from no points of
friction.

Developing a new language typically starts from a base language close to
the desired one. From there, a Racket developer creates a new language with
some or all of the following actions: 
@;
@itemlist[ 

@item{adding new linguistic constructs;}

@item{hiding linguistic constructs; and}

@item{re-interpreting linguistic constructs.}
]
Here, linguistic constructs are any functions or syntactic extensions
added to a language, such as list comprehensions.
For the last one, Racket developers heavily
rely on linguistic interposition points, that is, anchors in
the syntax elaboration process where a program may inject
additional syntax transformations. Video exploits all of the
above.

Due to the ease of developing and installing languages in
the Racket ecosystem, language creation has become a
critical ``warhead'' in the arsenal of software-engineering
tools, of comparable status as Haskell's type classes and
ML's functors. When developers realize that it is best to
express themselves in the language of a domain, they do not
hesitate to develop a matching programming language. After
all, domain experts have developed this specialized
terminology (and ontology) so that they can discuss problems
and solutions efficiently.@note{Not every DSL is a week or
 month-long project. Turnstile@cite[tsam-popl] took almost a
 year of development time!}

The domain of video editing is a particularly well-suited domain for
illustrating the above points. While the evolution of the language follows
the standard path from a veneer for a C library to a full-fledged
language@cite[fowler], Racket reduces this path significantly and this
pearl demonstrates how.  Before we can describe Video and its
implementation, however, we need to survey the world of editing videos.
