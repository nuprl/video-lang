#lang scribble/sigplan

@require[scriblib/footnote
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "related"]{The Related Suspects}

Two main principles guide The Racket Way@cite[racket-way] of creating DSLs:
@bold{reuse} and @bold{abstraction}. Reusing existing language components
greatly facilitates DSL @emph{creators}. As the old adage goes, "don't build
languages from scratch"@cite[hudak-dsl]. To aid reuse, the Racket compiler
allows componentwise overriding of every core form@cite[lal-pldi].

DSL @emph{users} are even further removed from the PL world, and thus deserve
proper abstractions (especially on top of reused components). In other words,
host language details should not leak into uses of the DSL, including in error
messages. Racket's syntactic abstraction capabilities builds on the
innovations of its Scheme and Lisp roots, but these predecessors never cared
about writing true abstractions. At best, programmers write low-level
validation code that often clutters the implementation; more commonly,
validation is omitted, leaving Lisp macros that resemble naive rewrite rules
that do not distinguish the DSL from the host language. Racket helps the
creation of robust linguistic abstractions with a declarative DSL for writing
macros@cite[fortifying-jfp]. Instead of low-level validation code, programmers
write high-level specifications, which is compiled to produce error messages in
terms of the surface language.

Some researchers@cite[deep-shallow-icfp] have tried to present an alternative
``deep'' vs ``shallow'' characterization of DSLs. This view seems to ignore
reuse and abstraction and thus seems removed from the original purpose of DSLs,
which is to help an ``end user'' solve a domain-specific
problem@cite[hudak-dsl]. Deep embeddings implement a DSL's AST as algebraic
datatype constructors. As a result, well-typed DSL terms are also well-typed
host terms. While this makes effective reuse of the host language's type
checker, this approach falls short on reuse because it requires implementing an
evaluator from scratch. In addition, the deep embedding approach requires
advanced type system features@cite[gadt-popl gadt-icfp], which often produce
obscure error messages that compromise the abstractions of the DSL.

The ``shallow'' approach uses standard functions to build the DSL
@cite[hudak-dsl] and thus both reuses the evaluator of the host and leverages
the abstraction power of functions and types to hide implementation
details. This approach has limited flexibility to deviate from and change
features of the host, however, since it does not consider syntactic abstraction
nor interposition of language features. In other words, programmers cannot
create @emph{new} abstractions. The ``finally tagless''
technique@cite[tagless-jfp] tries to improve on the shallow approach via a
clever encoding of types, but sacrifices reuse and abstraction altogether in
the process.
