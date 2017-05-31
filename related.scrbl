#lang scribble/acmart

@require[scriblib/footnote
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "related"]{The Related Suspects}

Two main principles guide the Racket Way@note{Matthew Flatt. ``The Racket
Way''. Strange Loop Conference, 2012. @url{infoq.com/presentations/Racket}} of
creating DSLs@cite[flatt-acm]: @bold{reuse} and @bold{abstraction}.
As the old adage goes, ``don't
build languages from scratch'' @cite[hudak-dsl].
Reusing existing language components greatly helps DSL @emph{creators}, who are
unlikely to be programming language researchers.
To aid reuse, Racket allows
componentwise interposition of every core form.

Like all programmers, DSL @emph{users} deserve proper abstractions, that is,
constructs that do not expose the underlying, reused components. In other words, host
language details should not leak into uses of the DSL, including in error
messages. Racket's syntactic abstraction capabilities builds on the innovations
of its Scheme and Lisp roots, but these predecessors never cared about writing
true abstractions. At best, Lisp and Scheme programmers write low-level
validation code that clutters the implementation; more commonly, validation is
omitted, leaving Lisp and Scheme macros that resemble naive rewrite rules that
do not distinguish the DSL from the host language. Racket helps the creation of
robust linguistic abstractions with a declarative DSL for writing syntax
transformers @cite[fortifying-jfp]. Instead of low-level validation code,
programmers write high-level specifications, which is compiled to produce error
messages in terms of the surface language.

Researchers have studied DSLs for a long time and have developed various
alternative classifications for DSL construction strategies. In the functional
programming language community, @citet[deep-shallow-icfp] distinguish DSLs
along ``deep'' vs ``shallow'' lines.  Deep embeddings implement a DSL's AST as
algebraic datatype constructors. As a result, well-typed DSL terms are also
well-typed host terms. While this makes effective reuse of the host language's
type checker, this approach falls short on reuse because it requires
implementing an evaluator from scratch. In addition, the deep embedding
approach requires advanced type system features@cite[gadt-popl gadt-icfp],
which often produce obscure error messages that compromise the abstractions of
the DSL.  The ``shallow'' approach uses standard functions to build DSLs
@cite[hudak-dsl] and thus both reuses the evaluator of the host and leverages
the abstraction power of functions and types to hide implementation
details. This approach has limited flexibility to deviate from and change
features of the host, however, because it does not consider syntactic
abstraction nor interposition of language features. In other words, programmers
cannot create @emph{new} abstractions. The ``finally tagless''
technique@cite[tagless-jfp] tries to improve on the shallow approach via a
clever encoding of types, but it sacrifices reuse and abstraction altogether in
the process.

In general, the deep vs shallow distinction seems more of an
academic analysis and less focused on the pragmatics of DSL
creation. Thus, it might not be so useful for programmers
who are weighing tradeoffs and trying to solve
domain-specific problems in the most effective manner. Some
industrial developers @cite[fowler] view DSLs as either
``internal'' or ``external.'' Internal DSLs focus on reuse
and typically involve extending a host language, which is
why they are also commonly called ``embedded'' DSLs. The
Racket approach creates internal DSLs. External DSLs, as
exemplified by the UNIX philosophy of ``little languages''
@cite[little-languages unix], are typically implemented from
scratch and require more effort but as a result are not
constrained by any particular host language.@note{Racket,
 like most languages, can of course be used to create
 external DSLs, but they do not represent ``the Racket
 way''.}

Racket may also be considered a ``language workbench''@note{``Language
Workbenches: The Killer-App for DSLs?'', 2005,
@url{martinfowler.com/articles/languageWorkbench.html}}
@cite[racket-workbench-challenge], which describes a broad spectrum of tools
and frameworks for creating DSLs, as well as a large community of researchers
and developers. While workbenches tend to utilize GUI tools @cite[metaedit],
and commonly create external DSLs @cite[spoofax], some more closely resemble
Racket's ``reuse and abstraction'' approach @cite[sugarj], but with different
design choices and thus trade-offs. See @citet[language-workbenches-survey]'s
recent summary for a detailed survey of the characteristics and design choices
in a large world of workbenches.
