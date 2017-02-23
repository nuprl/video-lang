#lang scribble/sigplan

@(require "bib.rkt" scriblib/footnote)

@title[#:tag "conclusion"]{Star Trek Beyond}

Imagine being Spok on the USS Enterprise. The ship's hyper-light-nano-pulsar
 sensors have discovered this paper. A first glance reveals a whole new,
 alternative future---language-oriented programming. Now use extremely
 rational thinking to reason through the consequences of this insight.

Clearly, this alternative world exists because developers can build
 languages with the same ease as they build libraries (modules, structures,
 functors) now. Some of these languages may sit at the surface, helping
 domain experts formulate partial solutions to facets of the overall
 problem---in their own languages. The language implementations will often
 just use conventional programming techniques; at other times, developers
 will create more languages to implement languages. In the end, the source
 code of applications will consist of deep hierarchies of languages, each layer
 closing the gap between the ordinary constructs of conventional language
 (folds, maps, monads, algebraic types) and the concepts found in a problem
 domain (video clips, transitions, filters; FFI protection, FFI bindings).
 The hierarchy underneath the Racket code base contains several dozen
 such languages. 

Two critical factors enable this brave new world of language-oriented
 programming in the Racket eco-system. The first one is that developing
 languages---@emph{real} languages---is a process without friction. A
 language developer can edit a language implementation in one Emacs buffer,
 save the file, and immediately run a second Emacs buffer with code written
 in the language of the first one. Furthermore, the Racket syntax
 system---with interposition points, advance syntax-transformer facilities,
 syntax modules and so on---allows an extreme degree of linguistic
 re-use. Indeed, because of this potential of re-use, developers do not
 hesitate to create languages for a single use.

The second factor is that Racket acts as a common substrate of the various
 languages. Eventually programs in these ``little'' languages are elaborated
 into core Racket programs. Their values become Racket values.@note{For lazy
 Racket, this statement holds technically but philosophically the values are
 one level off in the type hierarchy. As a result, modules in lazy languages
 cannot be composed with modules in strict languages as easily as this
 sentence claims.} As a result, developers can easily compose modules in
 distinct languages. In other words, software systems are compositions of
 many modules, each representing a solution of a facet of the problem, each
 written in its most appropriate language.

Racket is the host of this multi-lingual paradise, and Video is a great
 poem-level illustration of how this paradise works and what it
 promises. Editing videos makes it particularly easy to recognize the
 functional-declarative aspect of the process and to invent a concise
 syntax for it. Implementing the language seemed to demand a syntactic
 verbosity that ordinary abstractions cannot hide; implementing a one-shot
 language to implement the Video language was natural.

No, Racket by no means solves all problems that come with language-oriented
 programming. But, it sets itself apart from many other approaches in the
 same direction, and it already has numerous successes to show for.  We
 hope that functional programmers of all stripes recognize the beauty of
 LOP in general and Racket's approach in particular, and we invite them to
 translate them into their world.
