#lang scribble/acmart

@(require (except-in scribble/manual cite)
	  scriblib/figure "bib.rkt" "pictures.rkt" scriblib/footnote)

@title[#:tag "conclusion"]{Star Trek Beyond}

Imagine being Spock on the USS Enterprise. The ship's
 hyper-light-nano-pulsar sensor has discovered this paper. The discovery
 reveals a whole new, alternative future---Racket's approach to
 language-oriented programming@cite[lop-ward lop-dmitriev]---only a few
 light-years away. Now use extremely rational thinking to reason through
 the consequences of this insight.

Clearly, this alternative world encourages developers to build languages
 that are as close as possible to problem domains. In this context,
 ``language'' falls into the same class of concepts as language library or
 module, structure, or functor. Software systems will consist of a deep
 hierarchy of languages. Some of these languages may sit at the surface of
 the system, helping domain experts formulate partial solutions to facets
 of the overall problem---in their own domain languages. Others may sit
 below the surface, in the interior of the hierarchy, because the
 implementation of DSLs also pose domain-specific problems.  Each language
 will narrow the gap between the ordinary constructs of the underlying
 language (folds, maps, monads, algebraic types) and the concepts found in
 a problem domain (videos; type systems; FFI bindings).  The hierarchy
 underneath Racket contains several dozen such languages, each dedicated to
 a special purpose, but all of them sitting within the core language.

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
 modules.  Developers can link these modules, creating multi-lingual
 systems with a single host language. 

@figure["language-tower" @list{The tower of languages for Video} 
  #:style center-figure-style]{
  @language-tower[.8]}

Racket is the host of this multi-lingual paradise, and Video is a poetic
 illustration of how this paradise works and what it
 promises. @Figure-ref{language-tower} displays an organization diagram
 that summarizes the small language hierarchy underneath Video and its
 dependencies. The Video language itself exists because the domain of video
 editing calls out for a declarative scripting language. In other words,
 Video closes the gap between the domain expert, who wishes to composite
 video clips programmatically, and the scripting language, whose core
 provides nothing but functions and variables and list comprehension and
 similar linguistic features.

Similarly, a linguistic gap shows up for the implementation of Typed Video, an
 extension of Video. Here the domain expert is a type-system designer, who
 uses type-checking rules to design type systems. Once again, the gap between
 an ordinary functional language and this language of type-system designers
 is quite large. Hence our implementation uses another DSL to articulate our
 type system---as attachments to just those linguistic features for which we
 want types--- as much as possible in the language of type-system designers.

On the left side of the diagram, we see another extension of Video, the
 Video Docs language, which Video programmers can use to create integrated
 documentation. Like Typed Video, it extends Video and rests on another
 domain-specific language, Scribble, which is a general-purpose mark-up
 language for writing API documentation@cite[scribble-icfp]. While this
 extension is less complicated than the one on the right side, it is
 nevertheless worth mentioning because this domain is often dealt with as
 an after-thought. 

The language gap also shows up with the implementation of Video. The
 language's run-time system demands extensive checking of values that flow
 into the C-level primitives. One way to translate the language of the C
 documentation into Racket, is to add explicit checks inside Racket
 function definitions. Instead, we designed and implemented a DSL for
 dealing with just this insertion of checks for this specific library.

Finally, all of these languages make extensive use of yet another DSL,
 @racket[syntax-parse]. This language closes the gap between Racket's
 constructs for defining syntax transformers and the language developers
 have in mind. For example, developers know that one particular element of
 syntactic form must be an identifier while another must be a
 definition. Furthermore, they know that the error messages must come from
 the form itself, not the result of elaborating an instance of the
 form. The language of @racket[syntax-parse] provides all this and more,
 once again allowing developers to use the language they have in mind
 instead of just the underlying, raw core language. 

No, Racket by no means solves all problems that come with language-oriented
 programming. But, it sets itself apart from other approaches in the functional
 world, plus it already has numerous successes to show for. We hope that
 functional programmers of all stripes recognize the beauty of
 language-oriented-programming in general and Racket's approach in particular,
 and we invite them to translate them into their world.



