#lang scribble/acmart

@(require (except-in scribble/manual cite)
	  scriblib/figure "bib.rkt" "pictures.rkt" scriblib/footnote)

@title[#:tag "conclusion"]{Star Trek Beyond}

Imagine being Spock on the USS Enterprise. The ship's hyper-light-nano-pulsar
 sensors have discovered this paper. A first glance reveals a whole new,
 alternative future---language-oriented programming---only a few light-years
 away . Now use extremely rational thinking to reason through the
 consequences of this insight.

Clearly, this alternative world exists because developers can then build
 languages with ease to close the gap between problem domains and what
 languages usually provide in terms of linguistic constructs. In this
 context, ``language'' falls into the same class of concepts as language
 library or modules or structures or functor. Some of these languages may sit
 at the surface, helping domain experts formulate partial solutions to facets
 of the overall problem---in their own languages. The language
 implementations will often just use conventional programming techniques; at
 other times, developers will create more languages to implement
 languages. In the end, the source code of applications will consist of a
 deep hierarchy of languages, each layer closing the gap between the ordinary
 constructs of a conventional language (folds, maps, monads, algebraic types)
 and the concepts found in a problem domain (video clips, transitions,
 filters; FFI protection, FFI bindings).  The hierarchy underneath Racket
 contains several dozen such languages.

Two critical factors enable this brave new world of language-oriented
 programming in the Racket ecosystem. The first one is that developing
 languages---@emph{real} languages---is a process without friction. A
 language developer can edit a language implementation in one Emacs buffer,
 save the file, and immediately run a second Emacs buffer with code written
 in the language of the first one. Furthermore, the Racket syntax
 system---with interposition points, advance syntax-transformer facilities,
 syntax modules and so on---allows an extreme degree of linguistic
 re-use. Indeed, because of this potential of re-use, developers do not
 hesitate to create languages for a single use.

The second factor is that Racket acts as a common substrate. Eventually
 programs in these ``little'' languages are elaborated into core Racket
 programs. Their values become Racket values. As a result, developers can mostly compose modules in distinct
 languages, meaning software systems are compositions of many modules, each
 representing a solution of a facet of the problem, each written in its most
 appropriate language.

@figure["language-tower" @list{The tower of languages for Video} #:style center-figure-style]{
  @language-tower[.8]}

Racket is the host of this multi-lingual paradise, and Video is a poetic
 illustration of how this paradise works and what it
 promises. @Figure-ref{language-tower} displays an organization diagram that
 explains the small language hierarchy underneath Video. The Video language
 itself exists because the domain of video editing calls out for a
 declarative scripting language. In other words, Video closes the gap between
 the domain expert, who wishes to composite video clips programmatically, and
 the scripting language, whose core provides nothing but functions and
 variables and list comprehension and similar linguistic features.

Similarly, a linguistic gap shows up for the implementation of Typed Video, an
 extension of Video. Here the domain expert is a type-system designer, who
 uses type-checking rules to design type systems. Once again, the gap between
 an ordinary functional language and this language of type-system designers
 is quite large. Hence our implementation uses another DSL to articulate our
 type system---as attachments to just those linguistic features for which we
 want types--- as much as possible in the language of type-system designers.

As a matter of fact, the language gap shows up a third time with the
 implementation of Video. The language's run-time system demands extensive
 checking of values that flow into the C-level primitives. One way to
 translate the language of the C documentation into Racket, is to add
 explicit checks inside Racket function definitions. Instead, we designed and
 implemented a DSL for dealing with just this insertion of checks for this
 specific library.

Finally, all of these languages make extensive use of yet another DSL,
 @racket[syntax-parse]. This language closes the gap between Racket's
 constructs for defining syntax transformers and the language developers
 have in mind. For example, developers know that one particular element of
 syntactic form must be an identifier while another must be a
 definition. Furthermore, they know what the error messages must come from
 the form itself, not the result of elaborating an occurrence of the
 form. The language of @racket[syntax-parse] provides all this and more,
 once again allowing developers to use the language they have in mind
 instead of just the underlying, raw core language. 

No, Racket by no means solves all problems that come with language-oriented
 programming. But, it sets itself apart from other approaches in the functional
 world, plus it already has numerous successes to show for. We hope that
 functional programmers of all stripes recognize the beauty of
 language-oriented-programming in general and Racket's approach in particular,
 and we invite them to translate them into their world.



