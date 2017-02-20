#lang scribble/sigplan

@(require "bib.rkt")

@title[#:tag "conclusion"]{Star Trek Beyond}

When Racketeers create new languages, they do not write interpreters or
 compilers. Instead they @emph{reuse} an existing language and its features
 as much as possible, including the syntax and the run-time parts. As a
 result, the cost of creating, installing, and running programs in a real,
 working language is low. The Racket world dubs this idea ``linguistic
 re-use,'' and its inhabitants have practiced it for years@cite[SK-PhD].

The key to Racket's linguistic reuse is its modular syntax
 system@cite[macros-icfp]. Every module specifies its implementation
 language, and a language is merely a module that exports specific
 constructs. Hence, a developer can use the regular module export and
 import mechanisms to define most of a new language from a base
 language. The rest comes from new syntactic constructs and run-time
 elements. Installing such a new language takes as little effort as naming
 it in on the first line of a client module. Thus, a developer can create a
 language in one Emacs buffer and test it another one. The process is
 completely free of pain, which us why developers feel as free to develop a
 language as a library

In practice, developers proceed bottom-up. They find an existing library
 and API for a task. Next, they create Racket bindings for this API. As
 they create these bindings, they ensure to preserve the invariants of
 Racket. For example, if a function can segfault on some kind of data, the
 developer makes sure that the Racket wrapper never hands such data to the
 foreign function.  Finally, developers create functional front-end
 primitives and a syntax that makes programs in the new language concise.

Video represents an ideal application case to demonstrate this design
 recipe for languages. Editing videos makes it particularly easy to
 recognize the functional-declarative aspect of the process and to develop
 a invent a concise syntax for it. Even the addition of a rather useful
 type system took less than 12 hours. We hope that functional programmers
 of all stripes recognize the beauty of the Racket idea and invite them to
 translate the idea into their world. 
