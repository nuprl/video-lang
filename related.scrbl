#lang scribble/sigplan

@require[scriblib/footnote
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "related"]{The Related Suspects}

@section{Video Editing}

@section{DSL Design}

Two main principles guide The Racket Way of creating DSLs: @bold{reuse} and
@bold{abstraction}. Since DSL @emph{creators} are often not programming
language experts, reusing existing language components greatly facilitates DSL
creation. In other words, "don't build languages from
scratch"@cite[hudak-dsl]. To aid reuse, the Racket compiler allows
componentwise overriding of every core form@cite[lal-pldi]. Obligingly, our
Video language modifies the behavior of modules and function bodies to: (1)
allow video components to be defined out of order, and (2) implicitly collect
their contents into an aggregate video. In addition, Typed Video extends
function application and lambda in order to inject static type checking.


DSL @emph{users} are even further removed from the PL world, and thus creating
proper abstractions (e.g., on top of reused components) is perhaps even more
important. In other words, details of the host language should not leak into
use of the DSL, e.g., in error messages. In this area, Racket has focused on
creating syntactic abstractions, i.e., macros, building on the innovations of
its Scheme and Lisp roots. These predecessor languages, however, do not offer
much help in the way of writing actual abstractions, with standard examples
resembling not much more than rewrite rules that does not care to distinguish
the surface language from the host language. At best, programmers write
low-level validation code but this code usually winds up cluttering and
dominating most of the implementation and so the more common case is that the
validation code is left out. Racket introduces a declarative DSL for writing
macros@cite[stxparse-icfp] that automatically produces error messages, in terms
of the surface language, from a user-written specification, replacing the
manually-written low-level validation code.

Other languages offer powerful abstraction modalities, namely types, but
unfortunately seem to have lost sight of the ultimate purpose of DSLs: to be a
practical tool for solving problems. Much of the literature has been obsessed
with implementing a basic interpreter in a type-preserving manner, i.e.,
well-typed terms in the surface language are also well-typed in the host but
this requires inventing advanced new types@cite[gadt-popl gadt-icfp], or clever
encodings@cite[tagless-jfp].

Gibbons and Wu's@cite[deep-shallow-icfp] "deep" vs "shallow" DSLs seems to miss
the point of DSLs entirely, and instead seem to use DSLs as a cover to indulge
in more category theory nonsense. Their deep embedding means implementing the
AST of the surface language using algebraic datatypes. Unfortunately, this
approach satisfies neither reuse nor abstraction since running a DSL program
requires implementing a separate interpreter, and errors are often obscure and
not in terms of the surface language. The "shallow" approach uses ordinary
functions to build the DSL@cite[hudak-dsl] and seems more reasonable since it
reuses the evaluator of the host language, and leverages the abstraction power
of functions and types to hide implementation details. However, one can go too
far with the shallow approach as well@cite[tagless-jfp], if one gets too cute
with encoding types.
