#lang scribble/sigplan

@require[scriblib/footnote
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "related"]{The Related Suspects}

@section{Video Editing}

@section{DSL Design}

Two main principles guide The Racket Way@cite[racket-way] of creating
DSLs: @bold{reuse} and @bold{abstraction}. Since DSL @emph{creators} are often
not programming language experts, reusing existing language components greatly
facilitates DSL creation. In other words, "don't build languages from
scratch"@cite[hudak-dsl]. To aid reuse, the Racket compiler allows
componentwise overriding of every core form@cite[lal-pldi].

DSL @emph{users} are even further removed from the PL world, and thus creating
proper abstractions (e.g., on top of reused components) is perhaps even more
important. In other words, host language details should not leak into uses of
the DSL, e.g., in error messages. Racket's syntactic abstraction capabilities,
i.e., macros, builds on the innovations of its Scheme and Lisp roots, but these
predecessors surprisingly do not help in writing true abstractions. At best,
programmers write low-level validation code that often clutters the
implementation; more commonly, validation, and thus abstraction, is omitted,
leaving macros that resemble naive rewrite rules that do not distinguish the
DSL from the host language. Racket helps the creation of robust abstractions
with a declarative DSL for writing macros@cite[stxparse-icfp] that
automatically produces error messages, in terms of the surface language, from a
user-written specifications, replacing the manually-written low-level
validation code.

Some researchers@cite[deep-shallow-icfp] have tried to present an
alternative "deep" vs "shallow" characterization of DSLs. This view, however,
seems removed from the original purpose of DSLs, which is to help an "end user"
solve a domain-specific problem@cite[hudak-dsl]. Deep embeddings implement a
DSL's AST as algebraic datatype constructors. As a result, well-typed DSL terms
are also well-typed host terms. While this makes effective reuse of the type
checker, this approach still falls short on reuse since it requires
implementing a separate evaluator. This approach also requires advanced type
system features@cite[gadt-popl gadt-icfp], however, which often produce obscure
error messages that compromise the abstractions of the DSL.

The "shallow" approach (e.g., as presented in @cite[hudak-dsl]) uses standard
functions to build the DSL and thus both reuses the evaluator of the host and
leverages the abstraction power of functions and types to hide implementation
details. While this approach reuses the infrastructure of a host language, it
mostly has limited flexibility to change features of the host since it does not
consider syntactic abstraction. The "finally tagless"
technique@cite[tagless-jfp] tries to improve on the shallow approach via a
clever encoding of types, but sacrifices reuse and abstraction in the process.
